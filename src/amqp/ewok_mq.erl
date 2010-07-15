%%
-module(ewok_mq).

-include("ewok_amqp.hrl").

-export([start/0, stop/0]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3, 
	handle_call/3, handle_cast/2, handle_info/2]).

-define(SOCKET_OPTS, [binary, {packet, 0}, {active,false}, {nodelay, true}]).
-define(SOCKET_CLOSING_TIMEOUT, 1000).
-define(CLIENT_CLOSE_TIMEOUT, 5000).
-define(HANDSHAKE_RECEIVE_TIMEOUT, 60000).

-record(nc_state, {
	params = #amqp_params{},
	sock,
	main_reader_pid,
	channel0_writer_pid,
	channel0_framing_pid,
	channel_max,
	heartbeat,
	closing = false,
	channels = new_channel_dict()
}).

-record(nc_closing, {reason, close, from = none, phase = terminate_channels}).

%%---------------------------------------------------------------------------
%% gen_server callbacks
%%---------------------------------------------------------------------------

start() ->
	Args = #amqp_params{},
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

stop() ->
	gen_server:call(?MODULE, stop).
	
init(Opts) ->
    process_flag(trap_exit, true),
    State = handshake(#nc_state{params = Opts}),
    {ok, State}.

%%
handle_call({command, Command}, From, #nc_state{closing = Closing} = State) ->
    case Closing of
	false -> 	
		handle_command(Command, From, State);
	_ -> 
		{reply, closing, State}
    end.

%% 
handle_cast({method, Method, Content}, State) ->
    handle_method(Method, Content, State);

handle_cast(stop, State) ->
	{stop, normal, State}.

%% 
handle_info(time_out_waiting_for_close_ok = Msg,State = #nc_state{closing = Closing}) ->
	ewok_log:warn("Connection ~p closing: timed out waiting for 'connection.close_ok'.", [self()]),
    {stop, {Msg, closing_to_reason(Closing)}, State};

%% Standard handling of exit signals
handle_info({'EXIT', Pid, Reason}, State) ->
    handle_exit(Pid, Reason, State).

terminate(_Reason, #nc_state{channel0_framing_pid = Framing0Pid,
                             channel0_writer_pid = Writer0Pid,
                             main_reader_pid = MainReaderPid}) ->
    ok = terminate_channel_infrastructure(network, {Framing0Pid, Writer0Pid}),
    case MainReaderPid of
	undefined -> 
		ok;
	_ -> 
		MainReaderPid ! close,
		ok
    end.

code_change(_OldVsn, State, _Extra) ->
    State.

%%
handle_command({open_channel, ProposedNumber}, _From, State) ->
    {ChannelPid, NewChannels} =
        open_channel(ProposedNumber, network, {
			State#nc_state.sock, 
			State#nc_state.main_reader_pid
		}, State#nc_state.channels),
    {reply, ChannelPid, State#nc_state{channels = NewChannels}};
%%
handle_command({close, #'connection.close'{} = Close}, From, State) ->
	NewState = set_closing_state(flush, 
		#nc_closing{reason = app_initiated_close,close = Close, from = From},
		State),
    {noreply, NewState}.

%% Handling methods from broker
handle_method(#'connection.close'{} = Close, none, State) ->
	NewState = set_closing_state(abrupt,
		#nc_closing{reason = server_initiated_close, close = Close},
		State),
    {noreply, NewState};

handle_method(#'connection.close_ok'{}, none,
              State = #nc_state{closing = Closing}) ->
    #nc_closing{from = From,
                close = #'connection.close'{reply_code = ReplyCode}} = Closing,
    case From of
	none -> 
		ok;
	_ -> 
		gen_server:reply(From, ok)
    end,
    if ReplyCode =:= 200 -> {stop, normal, State};
       true              -> {stop, closing_to_reason(Closing), State}
    end.

%%---------------------------------------------------------------------------
%% Closing
%%---------------------------------------------------------------------------

%% Changes connection's state to closing.
%%
%% ChannelCloseType can be flush or abrupt
%%
%% The closing reason (Closing#nc_closing.reason) can be one of the following
%%     app_initiated_close - app has invoked the close/{1,3} command. In this
%%         case the close field is the method to be sent to the server after all
%%         the channels have terminated (and flushed); the from field is the
%%         process that initiated the call and to whom the server must reply.
%%         phase = terminate_channels | wait_close_ok
%%     internal_error - there was an internal error either in a channel or in
%%         the connection process. close field is the method to be sent to the
%%         server after all channels have been abruptly terminated (do not flush
%%         in this case).
%%         phase = terminate_channels | wait_close_ok
%%     server_initiated_close - server has sent 'connection.close'. close field
%%         is the method sent by the server.
%%         phase = terminate_channels | wait_socket_close
%%
%% The precedence of the closing MainReason's is as follows:
%%     app_initiated_close, internal_error, server_initiated_close
%% (i.e.: a given reason can override the currently set one if it is later
%% mentioned in the above list). We can rely on erlang's comparison of atoms
%% for this.
set_closing_state(ChannelCloseType, Closing, 
                  #nc_state{closing = false,
                            channels = Channels} = State) ->
    broadcast_to_channels(
        {connection_closing, ChannelCloseType, closing_to_reason(Closing)},
        Channels),
    check_trigger_all_channels_closed_event(State#nc_state{closing = Closing});
%% Already closing, override situation
set_closing_state(ChannelCloseType, NewClosing,
                  #nc_state{closing = CurClosing,
                            channels = Channels} = State) ->
    %% Do not override reason in channels (because it might cause channels to
    %% to exit with different reasons) but do cause them to close abruptly
    %% if the new closing type requires it
	case ChannelCloseType of
	abrupt ->
		broadcast_to_channels(
			{connection_closing, ChannelCloseType,
			closing_to_reason(CurClosing)},
			Channels);
	_ -> 
		ok
   end,
   #nc_closing{reason = NewReason, close = NewClose} = NewClosing,
   #nc_closing{reason = CurReason} = CurClosing,
   ResClosing =
		if
           %% Override (rely on erlang's comparison of atoms)
           NewReason >= CurReason ->
               %% Note that when overriding, we keep the current phase
               CurClosing#nc_closing{reason = NewReason, close = NewClose};
           %% Do not override
           true ->
               CurClosing
		end,
    NewState = State#nc_state{closing = ResClosing},
    %% Now check if it's the case that the server has sent a connection.close
    %% while we were in the closing state (for whatever reason). We need to
    %% send connection.close_ok (it might be even be the case that we are
    %% sending it again) and wait for the socket to close.
    case NewReason of
	server_initiated_close -> 
		all_channels_closed_event(NewState);
	_ -> 
		NewState
    end.

%% The all_channels_closed_event is called when all channels have been closed
%% after the connection broadcasts a connection_closing message to all channels
all_channels_closed_event(#nc_state{channel0_writer_pid = Writer0,
                                    main_reader_pid = MainReader,
                                    closing = Closing} = State) ->
    #nc_closing{reason = Reason, close = Close} = Closing,
    case Reason of
	server_initiated_close ->
		do(network, Writer0, #'connection.close_ok'{}, none),
		erlang:send_after(?SOCKET_CLOSING_TIMEOUT, MainReader, socket_closing_timeout),
		State#nc_state{closing = Closing#nc_closing{phase = wait_socket_close}};
	_ ->
		do(network, Writer0, Close, none),
		erlang:send_after(?CLIENT_CLOSE_TIMEOUT, self(), timeout_waiting_for_close_ok),
		State#nc_state{closing = Closing#nc_closing{phase = wait_close_ok}}
    end.

closing_to_reason(#nc_closing{reason = Reason, close = Close}) ->
	#'connection.close'{reply_code = Code, reply_text = Text} = Close,
    {Reason, Code, Text}.

internal_error_closing() ->
	Close = #'connection.close'{reply_text = <<>>, reply_code = ?INTERNAL_ERROR, class_id = 0, method_id = 0},
    #nc_closing{reason = internal_error, close = Close}.

%%---------------------------------------------------------------------------
%% Channel utilities
%%---------------------------------------------------------------------------

check_trigger_all_channels_closed_event(#nc_state{closing = false} = State) ->
    State;
check_trigger_all_channels_closed_event(#nc_state{channels = Channels,
                                                  closing = Closing} = State) ->
    #nc_closing{phase = terminate_channels} = Closing, % assertion
    case is_channel_dict_empty(Channels) of
        true  -> all_channels_closed_event(State);
        false -> State
    end.

%%---------------------------------------------------------------------------
%% Trap exits
%%---------------------------------------------------------------------------

%% Handle exit from writer0
handle_exit(Writer0Pid, Reason, #nc_state{channel0_writer_pid = Writer0Pid} = State) ->
    ewok_log:warn("Connection (~p) closing: received exit signal from writer0. "
		"Reason: ~p~n", [self(), Reason]),
    {stop, {writer0_died, Reason}, State};

%% Handle exit from framing0
handle_exit(Framing0Pid, Reason, #nc_state{channel0_framing_pid = Framing0Pid} = State) ->
    ewok_log:warn("Connection (~p) closing: received exit signal from framing0. "
		"Reason: ~p~n", [self(), Reason]),
    {stop, {framing0_died, Reason}, State};

%% Handle exit from main reader
handle_exit(MainReaderPid, Reason, #nc_state{main_reader_pid = MainReaderPid, closing = Closing} = State) ->
    case {Closing, Reason} of
	%% Normal server initiated shutdown exit (socket has been closed after
	%% replying with 'connection.close_ok')
	{#nc_closing{reason = server_initiated_close, phase = wait_socket_close}, socket_closed} ->
		{stop, closing_to_reason(Closing), State};
	%% Timed out waiting for socket to close after replying with
	%% 'connection.close_ok'
	{#nc_closing{reason = server_initiated_close, phase = wait_socket_close}, socket_closing_timeout} ->
		ewok_log:warn("Connection (~p) closing: timed out waiting for socket "
			"to close after sending 'connection.close_ok", [self()]),
		{stop, {Reason, closing_to_reason(Closing)}, State};
        %% Main reader died
	_ ->
		ewok_log:warn("Connection (~p) closing: received exit signal from main "
			"reader. Reason: ~p~n", [self(), Reason]),
		{stop, {main_reader_died, Reason}, State}
    end;

%% Handle exit from channel or other pid
handle_exit(Pid, Reason, #nc_state{channels = Channels} = State) ->
    case is_channel_registered({chpid, Pid}, Channels) of
	true  -> 
		handle_channel_exit(Pid, Reason, State);
	false -> 
		handle_other_pid_exit(Pid, Reason, State)
    end.

%% Handle channel exit
handle_channel_exit(Pid, Reason, #nc_state{closing = Closing} = State) ->
    case Reason of
	%% Normal amqp_channel shutdown
	normal ->
		{noreply, unregister_channel(Pid, State)};
	%% Channel terminating because of connection_closing
	{_Reason, _Code, _Text} when Closing =/= false ->
		{noreply, unregister_channel(Pid, State)};
	%% Channel terminating (server sent 'channel.close')
	{server_initiated_close, _Code, _Text} ->
		%% TODO determine if it's either a soft or a hard error. Terminate
		%% connection immediately if it's a hard error
		{noreply, unregister_channel(Pid, State)};
	%% amqp_channel dies with internal reason - this takes the entire
	%% connection down
	_ ->
		ewok_log:warn("Connection (~p) closing: channel (~p) died. Reason: ~p~n",
					[self(), Pid, Reason]),
		State1 = unregister_channel(Pid, State),
		State2 = set_closing_state(abrupt, internal_error_closing(), State1),
		{noreply, State2}
    end.

%% Handle other pid exit
handle_other_pid_exit(Pid, Reason, State) ->
    ewok_log:warn("Connection (~p) closing: received unexpected exit signal "
              "from (~p). Reason: ~p~n", [self(), Pid, Reason]),
    {noreply, set_closing_state(abrupt, internal_error_closing(), State)}.

%%---------------------------------------------------------------------------
%% Handshake
%%---------------------------------------------------------------------------

handshake(State = #nc_state{params = #amqp_params{host = Host, port = Port,
                                                  ssl_options = none}}) ->
    case gen_tcp:connect(Host, Port, ?SOCKET_OPTS) of
	{ok, Socket} -> 
		do_handshake(State#nc_state{sock = Socket});
	{error, Reason} -> 
		ewok_log:warn({"Could not start the network driver", Reason}),
		exit(Reason)
    end;
handshake(State = #nc_state{params = #amqp_params{host = Host, port = Port,
                                                  ssl_options = SslOpts}}) ->
	crypto:start(),
	ssl:start(),
    case gen_tcp:connect(Host, Port, ?SOCKET_OPTS) of
	{ok, Socket} ->
		case ssl:connect(Socket, SslOpts) of
		{ok, SslSocket} ->
			RabbitSslSocket = #ssl_socket{ssl = SslSocket, tcp = Socket},
			do_handshake(State#nc_state{sock = RabbitSslSocket});
		{error, Reason} ->
			ewok_log:warn({"Could not upgrade the network driver to ssl", Reason}),
			exit(Reason)
		end;
	{error, Reason} ->
		ewok_log:error({"Could not start the network driver", Reason}),
		exit(Reason)
    end.

do_handshake(State0 = #nc_state{sock = Socket}) ->
    ok = rabbit_net:send(Socket, ?PROTOCOL_HEADER),
    {Framing0Pid, Writer0Pid} =
        start_channel_infrastructure(network, 0, {Socket, none}),
    MainReaderPid = amqp_main_reader:start(Socket, Framing0Pid),
    State1 = State0#nc_state{channel0_framing_pid = Framing0Pid,
                             channel0_writer_pid = Writer0Pid,
                             main_reader_pid = MainReaderPid},
    State2 = network_handshake(State1),
    MainReaderPid ! {heartbeat, State2#nc_state.heartbeat},
    State2.

network_handshake(State = #nc_state{channel0_writer_pid = Writer0,
                                    params = Params}) ->
    #'connection.start'{} = handshake_recv(State),
    do(network, Writer0, start_ok(State), none),
    #'connection.tune'{channel_max = ChannelMax,
                       frame_max = FrameMax,
                       heartbeat = Heartbeat} = handshake_recv(State),
    TuneOk = #'connection.tune_ok'{channel_max = ChannelMax,
                                   frame_max = FrameMax,
                                   heartbeat = Heartbeat},
    do(network, Writer0, TuneOk, none),
    %% This is something where I don't understand the protocol,
    %% What happens if the following command reaches the server
    %% before the tune ok?
    %% Or doesn't get sent at all?
    ConnectionOpen = #'connection.open'{virtual_host = Params#amqp_params.virtual_host},
    do(network, Writer0, ConnectionOpen, none),
    #'connection.open_ok'{} = handshake_recv(State),
    %% TODO What should I do with the KnownHosts?
    State#nc_state{channel_max = ChannelMax, heartbeat = Heartbeat}.

start_ok(#nc_state{params = #amqp_params{username = Username,
                                         password = Password}}) ->
    %% TODO This eagerly starts the amqp_client application in order to
    %% to get the version from the app descriptor, which may be 
    %% overkill - maybe there is a more suitable point to boot the app
    rabbit_misc:start_applications([amqp_client]),
    {ok, Vsn} = application:get_key(amqp_client, vsn),
    LoginTable = [{<<"LOGIN">>, longstr, Username},
                  {<<"PASSWORD">>, longstr, Password}],
    #'connection.start_ok'{
        client_properties = [
            {<<"product">>, longstr, <<"Ewok">>},
            {<<"version">>, longstr, list_to_binary(Vsn)},
            {<<"platform">>, longstr, <<"Erlang">>}
		],
        mechanism = <<"AMQPLAIN">>,
        response = rabbit_binary_generator:generate_table(LoginTable)}.

handshake_recv(#nc_state{main_reader_pid = MainReaderPid}) ->
    receive
        {'$gen_cast', {method, Method, _Content}} ->
            Method;
        {'EXIT', MainReaderPid, Reason} ->
            exit({main_reader_died, Reason})
    after ?HANDSHAKE_RECEIVE_TIMEOUT ->
        exit(awaiting_response_from_server_timed_out)
    end.

%%---------------------------------------------------------------------------
%% Opening channels
%%---------------------------------------------------------------------------

%% Spawns a new channel process linked to the calling process and registers it
%% in the given Channels dict
open_channel(ProposedNumber, Driver, StartArgs, Channels) ->
    ChannelNumber = assign_channel_number(ProposedNumber, Channels),
    {ok, ChannelPid} = gen_server:start_link(amqp_channel, {self(), ChannelNumber, Driver, StartArgs}, []),
    #'channel.open_ok'{} = amqp_channel:call(ChannelPid, #'channel.open'{}),
    NewChannels = register_channel(ChannelNumber, ChannelPid, Channels),
    {ChannelPid, NewChannels}.

assign_channel_number(none, Channels) ->
    %% TODO Implement support for channel_max from 'connection.tune'
    %% TODO Make it possible for channel numbers to be reused properly
    get_max_channel_number(Channels) + 1;
assign_channel_number(ChannelNumber, Channels) ->
    case is_channel_registered({channel, ChannelNumber}, Channels) of
        true  -> assign_channel_number(none, Channels);
        false -> ChannelNumber
    end.

%%---------------------------------------------------------------------------
%% Starting and terminating channel infrastructure
%%---------------------------------------------------------------------------

start_channel_infrastructure(network, ChannelNumber, {Sock, MainReader}) ->
    FramingPid = rabbit_framing_channel:start_link(fun(X) -> X end, [self()]),
    WriterPid = rabbit_writer:start_link(Sock, ChannelNumber, ?FRAME_MIN_SIZE),
    case MainReader of
        none ->
            ok;
        _ ->
            MainReader ! {register_framing_channel, ChannelNumber, FramingPid,
                          self()},
            MonitorRef = erlang:monitor(process, MainReader),
            receive
			registered_framing_channel ->
				erlang:demonitor(MonitorRef), ok;
			{'DOWN', MonitorRef, process, MainReader, _Info} ->
				erlang:error(main_reader_died_while_registering_framing)
            end
    end,
    {FramingPid, WriterPid};
start_channel_infrastructure(direct, ChannelNumber, #amqp_params{username = User, virtual_host = VHost}) ->
    Peer = rabbit_channel:start_link(ChannelNumber, self(), self(), User, VHost),
    {Peer, Peer}.

terminate_channel_infrastructure(network, {FramingPid, WriterPid}) ->
    rabbit_framing_channel:shutdown(FramingPid),
    rabbit_writer:shutdown(WriterPid),
    ok;
terminate_channel_infrastructure(direct, {Peer, Peer})->
    gen_server2:cast(Peer, terminate),
    ok.

%%---------------------------------------------------------------------------
%% Do
%%---------------------------------------------------------------------------

do(network, Writer, Method, Content) ->
    case Content of
	none -> 
		rabbit_writer:send_command_and_signal_back(Writer, Method,
                                                           self());
	_  -> 
		rabbit_writer:send_command_and_signal_back(Writer, Method,
                                                           Content, self())
    end,
    receive_writer_send_command_signal(Writer);
do(direct, Writer, Method, Content) ->
    case Content of
	none -> 
		rabbit_channel:do(Writer, Method);
	_ -> 
		rabbit_channel:do(Writer, Method, Content)
    end.

receive_writer_send_command_signal(Writer) ->
    receive
	rabbit_writer_send_command_signal -> 
		ok;
	WriterExitMsg = {'EXIT', Writer, _} -> 
		self() ! WriterExitMsg
    end.

%%---------------------------------------------------------------------------
%% Channel number/pid registration
%%---------------------------------------------------------------------------

%% New channel dictionary for keeping track of the mapping between the channel
%% pid's and the channel numbers (the dictionary will essentially be used as a
%% bimap)
new_channel_dict() ->
    dict:new().

%% Returns true iff there are no channels currently registered in the given
%% dictionary
is_channel_dict_empty(Dict) ->
    dict:size(Dict) =:= 0.

%% Register a channel in a given channel dictionary
register_channel(Number, Pid, Dict) ->
    case dict:is_key({channel, Number}, Dict) of
        true ->
			ewok_log:error({channel_already_registered, Number});
        false ->
            Dict1 = dict:store({channel, Number}, {chpid, Pid}, Dict),
            dict:store({chpid, Pid}, {channel, Number}, Dict1)
    end.

%% Unregister a channel by passing either {channel, Number} or {chpid, Pid} for
%% Channel
unregister_channel(Pid, State = #nc_state{channels = Channels}) ->
    NewChannels = unregister_channel({chpid, Pid}, Channels),
    NewState = State#nc_state{channels = NewChannels},
    check_trigger_all_channels_closed_event(NewState);
unregister_channel(Channel, Dict) ->
    case dict:fetch(Channel, Dict) of
	undefined -> 
		ewok_log:error({channel, undefined});
	Val -> 
		dict:erase(Val, dict:erase(Channel, Dict))
    end.

%% Resolve channel by passing either {channel, Number} or {chpid, Pid} for
%% Channel - UNUSED
%resolve_channel(Channel, Dict) ->
%    dict:fetch(Channel, Dict).

%% Returns true iff Channel is registered in the given channel dictionary.
%% Pass either {channel, Number} or {chpid, Pid} for Channel
is_channel_registered(Channel, Dict) ->
    dict:is_key(Channel, Dict).

%% Returns the greatest channel number of the currently registered channels in
%% the given dictionary. Returns 0 if there are no channels registered.
get_max_channel_number(Dict) ->
    dict:fold(fun({channel, N}, _,  Max) when Max >= N -> Max;
                 ({channel, N}, _, _Max)               -> N;
                 ({chpid,   _}, _,  Max)               -> Max
              end, 0, Dict).

%%---------------------------------------------------------------------------
%% Other channel utilities
%%---------------------------------------------------------------------------

broadcast_to_channels(Message, Dict) ->
    dict:map(fun({chpid, Channel}, _) -> Channel ! Message, ok;
                ({channel, _}, _)     -> ok
             end, Dict),
    ok.

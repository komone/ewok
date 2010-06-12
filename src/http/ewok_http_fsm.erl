-module(ewok_http_fsm).

-compile(export_all).

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_service).
-export([start_link/1, stop/0]).
-export([service/2]).

-behaviour(gen_fsm).
-export([init/1, handle_info/3, handle_event/3, 
	handle_sync_event/4, terminate/3, code_change/4]).	

-record(state, {id, user, socket, max_headers, remote_ip, request}).

start_link(_Args) -> 
	try begin
		ewok_log:message(service, ?MODULE),
		%% DS = ewok_data_srv:connect(default)
		Transport = gen_tcp, %%
		Port = ewok:config({ewok, http2, port}, 8081),
		SocketOpts = ewok_socket:configure(Transport, {ewok, http2}),
		MaxConnections = ewok:config({ewok, http2, max_connections}, infinity),
		Timeout = ewok:config({ewok, http2, timeout}, 30) * 1000,
		
		Handler = fun(X) -> ?MODULE:service(X, Timeout) end,
		
		Configuration = [
			{name, ?MODULE},
			{transport, Transport}, %% defines use of gen_tcp or ssl module
			{port, Port},
			{protocol, http},
			{max_connections, MaxConnections},
			{socket_opts, SocketOpts},
			{handler, Handler}
		],
		%% ?TTY("CONFIG:~n~p~n", [Configuration]),
		ewok_log:message(configuration, {?MODULE, Configuration}),
		%% Starts a TCP Server for UTP
		{ok, Pid} = ewok_socket_srv:start_link(?MODULE, Configuration),
		ewok_log:add_log(?MODULE),
		{ok, Pid}
	end catch
	Error:Reason -> {Error, Reason}
	end.

stop() -> 
	ewok_socket_srv:stop(?MODULE),
	ewok_log:remove_log(?MODULE).


%% Callback from ewok_socket_srv, handing over a client connection
service(Socket, Timeout) ->
	{ok, Session} = create_session([]),
	ewok_log:message(?MODULE, {session, [{pid, Session}, {peer, ewok_socket:peername(Socket)}]}),
	?TTY({session, Session}),
	service(Socket, Session, 1, Timeout).
	
%%
service(Socket, Session, Sequence, Timeout) ->
	ok = ewok_socket:setopts(Socket, [{packet, line}]),
	case ewok_socket:recv(Socket, 0, Timeout) of
	{ok, RequestLine} ->
		try begin
			[Method, Path, <<"HTTP/", Version/binary>>] = ewok_text:split(RequestLine, <<"[ \r\n]">>),
			Headers = read_headers(Socket, Timeout, []),
			ok = ewok_socket:setopts(Socket, [{packet, raw}]),
			case request(Session, {{Method, Path, Version}, Headers}) of
			{ok, Response} ->
				ok = ewok_socket:send(Socket, Response),
				service(Socket, Session, Sequence + 1, Timeout);
			continue ->
				service(Socket, Session, Sequence + 1, Timeout);
			close ->
				?TTY("CLOSING"),
				close(Session),
				ewok_socket:close(Socket),
				exit(normal)
			end
		end catch 
		Error:Reason ->
			?TTY({?MODULE, error, {Error, Reason}})
			%reply(Socket, {Error, Reason})
		end;
	{error, Reason} ->
		?TTY({error, Reason})
	end.
%%
read_headers(Socket, Timeout, Acc) ->
	case ewok_socket:recv(Socket, 0, Timeout) of
	{ok, <<"\r\n">>} ->
		lists:reverse(Acc);
	{ok, Bin} -> 
		[Name, Value] = ewok_text:split(Bin, <<":">>, 2),
		read_headers(Socket, Timeout, [{ewok_text:trim(Name), ewok_text:trim(Value)}|Acc])
	end.

%% API
create_session(Options) ->
    gen_fsm:start_link(?MODULE, [], Options).
	
%%
request(Pid, Request) ->
	gen_fsm:sync_send_event(Pid, Request).

%%
close(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).


%% callbacks: gen_fsm

init(Options) ->
    process_flag(trap_exit, true), %% should we trap exits?
	_Timeout = proplists:get_value(timeout, Options, 180),
	{ok, connected, #state{}}.

connected(Request, _From, State) ->
	?TTY({request, Request}),
	ewok_log:message(?MODULE, {request, Request}),
	{reply, close, connected, State}.

websocket(Request, _From, State) ->
	{reply, ok, websocket, State}.
	
%%
handle_event(stop, _StateName, State) ->
	ewok_log:message(?MODULE, stop),
	{stop, normal, State};
handle_event(Event, StateName, State) ->
	ewok_log:message(?MODULE, event, Event),
    {stop, {StateName, undefined_event, Event}, State}.
%
handle_sync_event(Event, _From, StateName, State) ->
	ewok_log:message(?MODULE, error, Event),
    {stop, {StateName, undefined_event, Event}, State}.
%
handle_info(Info, StateName, State) ->
	ewok_log:message(?MODULE, info, Info),
    {noreply, StateName, State}.
%
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
%
terminate(_Reason, _StateName, _State) ->
    ok.

%% This started as mochiweb_socket_server (believe it or not)
-module(ewok_tcp_srv).
-vsn("1.0").
-author('steve@simulacity.com').

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, code_change/3, terminate/2]).

-export([start_link/2, stop/1]).
-export([accept/1]).

% gen_server

-record(state, {
	name,
	port, 
	ip=any,
	socket=null,
	protocol,
	handler,
	pid=null,
	count=0,
	max=infinity
}).

%%	
start_link(Name, Opts) when is_atom(Name), is_list(Opts) ->
	gen_server:start_link({local, Name}, ?MODULE, [{name, Name}|Opts], []).
%
stop(Name) when is_atom(Name) -> gen_server:cast(Name, stop);
stop(Pid) when is_pid(Pid) -> gen_server:cast(Pid, stop).

%%
%% gen_server
%%
%%
init(Opts) ->
    process_flag(trap_exit, true),
	Ip = ewok_config:get("ewok.ip", any),
	SocketOpts = [
		{ip, Ip},
		ewok_config:get("ewok.http.tcp.socket.type", binary),
		{ reuseaddr, ewok_config:get("ewok.http.tcp.socket.reuseaddr", true) },
		{ packet, ewok_config:get("ewok.http.tcp.socket.packet", 0) },
		{ backlog, ewok_config:get("ewok.http.tcp.socket.backlog", 30) },
		{ recbuf, ewok_config:get("ewok.http.tcp.socket.recbuf", 8192) },
		{ active, ewok_config:get("ewok.http.tcp.socket.active", false) },
		{ nodelay, ewok_config:get("ewok.http.tcp.socket.nodelay", true) }
	],
	Name = proplists:get_value(name, Opts),
	Port = proplists:get_value(port, Opts, 0),
	Protocol = proplists:get_value(protocol, Opts, http),
	Handler = proplists:get_value(handler, Opts),
	MaxConnections = ewok_config:get("ewok.tcp.max_connections", infinity),
	%?TTY("Listen port ~p (~p)~nOpts: ~p~n", [Port, Handler, SocketOpts]), 
	case gen_tcp:listen(Port, SocketOpts) of
	{ok, ServerSocket} ->
		{ok, Port} = inet:port(ServerSocket),
		{ok, listen(#state{name=Name, ip=Ip, socket=ServerSocket, port=Port, 
			max=MaxConnections, protocol=Protocol, handler=Handler})};
	{error, Reason} ->
		{stop, Reason}
    end.
	
listen(S = #state{socket=ServerSocket, handler=Handler, count=Count, max=Max}) ->
	case Count < Max of
	true ->
		Pid = proc_lib:spawn_link(?MODULE, accept, [{self(), ServerSocket, Handler}]),
		S#state{pid=Pid};
	false ->
		error_logger:info_report([{application, ?MODULE}, "Not accepting new connections", S]),
		S#state{pid=null}
	end.

accept({Pid, ServerSocket, Handler}) ->
	%?TTY("ACCEPT: ~p~n", [{Pid, ServerSocket, inet:getstat(ServerSocket)}]),
    case catch gen_tcp:accept(ServerSocket) of
	{ok, Socket} ->
		gen_server:cast(Pid, {accepted, self()}),
		Handler(Socket);
	{error, closed} ->
		exit({error, closed});
	Other ->
		Reason = lists:flatten(io_lib:format("~p", [Other])), 
		error_logger:error_report([{application, ewok}, "Accept failed error", Reason]),
		exit({error, accept_failed})
    end.

%
handle_call(_Message, _From, State) ->
    {noreply, State}.
%
handle_cast({accepted, Pid}, State=#state{pid=Pid, count=Count}) ->
    NewState = State#state{count=Count + 1},
    {noreply, listen(NewState)};
handle_cast(stop, State) ->
    {stop, normal, State}.
%
handle_info({'EXIT', Pid, normal}, State=#state{pid=Pid}) ->
    {noreply, listen(State)};
handle_info({'EXIT', Pid, Reason}, State=#state{pid=Pid}) ->
    error_logger:error_report({?MODULE, ?LINE, {acceptor_error, Reason}}),
    timer:sleep(100),
    {noreply, listen(State)};
handle_info({'EXIT', _LoopPid, Reason}, State=#state{pid=Pid, count=Count}) ->
    case Reason of 
	normal -> ok;
	_ -> error_logger:error_report({?MODULE, ?LINE, {child_error, Reason}})
    end,
	NewState = State#state{count=Count-1},
    FinalState = 
		case Pid of
		null -> listen(NewState);
		_ -> NewState
		end,
    {noreply, FinalState};
handle_info(Info, State) ->
    error_logger:info_report([{info, Info}, {state, State}]),
    {noreply, State}.
%	
code_change(_OldVsn, State, _Extra) ->
    State.
%
terminate(_Reason, #state{socket=Socket}) ->
    gen_tcp:close(Socket).

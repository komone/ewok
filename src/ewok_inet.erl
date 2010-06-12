%%
-module(ewok_inet).
-include("ewok.hrl").
-include("ewok_system.hrl").

-export([start_link/1, stop/1, service/4, behaviour_info/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, code_change/3, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {mod, callback, data}).

behaviour_info(callbacks) -> 
	[{init, 1}, {terminate, 3}];
behaviour_info(_) -> 
	undefined.

%%
start_link(Service = #ewok_inet{timeout=Timeout, handler=Handler, codec=Codec}) ->
	SocketHandler = fun(Socket) -> service(Socket, Handler, Codec, Timeout) end,
	%% messy
	Opts = [
		{name, Service#ewok_inet.name},
		{protocol, Service#ewok_inet.protocol},
		{port, Service#ewok_inet.port},
		{transport, Service#ewok_inet.transport},
		{socket_opts, Service#ewok_inet.socket_opts},
		{max_connections, Service#ewok_inet.max_connections},
		{handler, SocketHandler}
	],
	ewok_socket_srv:start_link(?MODULE, Opts).
%%
stop(Pid) ->
	gen_server:cast(Pid, stop),
	ewok_socket_srv:stop(Pid).

%%
service(Socket, Handler, _Codec, Timeout) ->
    {ok, Session} = gen_server:start_link(?MODULE, Handler, []),
	?TTY({Handler, session, Session}),
	ewok_log:message({Handler, {pid, Session}, {peer, ewok_socket:peername(Socket)}}),
	loop(Socket, Session, [], Timeout).
	
loop(Socket, Session, Request, Timeout) ->
	case gen_server:call(Session, Request) of
	{noreply, SocketOpts} ->
		Length = proplists:get_value(length, SocketOpts, 0),
		{ok, NewData} = ewok_socket:recv(Socket, Length, Timeout),
		loop(Socket, Session, NewData, Timeout);	
	{reply, Response, SocketOpts} ->	
		ewok_socket:send(Socket, Response),
		Length = proplists:get_value(length, SocketOpts, 0),
		{ok, NewData} = ewok_socket:recv(Socket, Length, Timeout),
		loop(Socket, Session, NewData, Timeout);
	close ->
		gen_server:cast(Session, stop),
		ewok_socket:close(Socket),
		exit(normal)
	end.
	
%% callbacks: gen_server - connection
init(Handler) ->
	process_flag(trap_exit, true), %% should we trap exits?
	case Handler:init([]) of
	{ok, Callback, Data} ->
		{ok, {Handler, Callback, Data}};
	Value ->
		Value
	end.
%
handle_call(Message, _From, {Handler, Callback, Data}) ->
	case Handler:Callback(Message, Data) of
	{noreply, Callback2, Data2} ->
		{reply, {noreply, []}, {Handler, Callback2, Data2}};
	{reply, Reply, Callback2, Data2} ->
		{reply, {reply, Reply, []}, {Handler, Callback2, Data2}};
	{disconnect, Callback2, Data2} ->
		{reply, close, {Handler, Callback2, Data2}}
	end.
%
handle_cast(stop, State) ->
    {stop, normal, State}.
%
handle_info(Info, State) ->
    ewok_log:info([{info, Info}, {state, State}]),
    {noreply, State}.
%	
code_change(_OldVsn, State, _Extra) ->
    State.
%
terminate(Reason, {Handler, Callback, Data}) ->
	Handler:terminate(Reason, Callback, Data).


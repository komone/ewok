%% Copyright 2009 Steve Davis <steve@simulacity.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ewok_socket_srv).
-vsn("1.0").
-author('steve@simulacity.com').

%% TEMP!!! for TTY
-include("ewok.hrl").

-behaviour(gen_server2).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, code_change/3, terminate/2]).

-export([configure/2, connections/1, start_link/2, stop/1]).
-export([accept/1]).

% internal state record
-record(state, {
	name,
	port, 
	ip={0,0,0,0},
	socket=null,
	protocol,
	handler,
	pid=null,
	count=0,
	max=infinity
}).

%
configure(Transport, Prefix) when is_atom(Transport) ->
	%% IMPL: note that 'Transport' is the actual **module name**
	SocketOpts = [
		ewok:config(Prefix ++ ".tcp.socket.mode", binary),
		{ ip, ewok:config({ewok, ip}, {0, 0, 0, 0}) }, 
		{ packet, ewok:config(Prefix ++ ".tcp.socket.packet", 0) },
		{ backlog, ewok:config(Prefix ++ ".tcp.socket.backlog", 0) },
		{ active, ewok:config(Prefix ++ ".tcp.socket.active", false) },
		{ nodelay, ewok:config(Prefix ++ ".tcp.socket.nodelay", true) },
		{ reuseaddr, ewok:config(Prefix ++ ".tcp.socket.reuseaddr", true) }
	],
	ExtraOpts = 
		case ewok:config(Prefix ++ ".ssl.enabled", false) of
		true ->
			ssl:start(), %% TODO: is this really the right place to be starting ssl?
			[ 
				{ssl_impl, new}, %% NOTE: ONLY SUPPORT NEW_SSL 
				{ verify, ewok:config(Prefix ++ ".ssl.verify", 0) },
				{ depth, ewok:config(Prefix ++ ".ssl.depth", 1) },
				%{ password, ewok:config("ewok.http.ssl.password", undefined) }, %% TODO: ONLY SET if keyfile is protected
				{ keyfile, ewok:config(Prefix ++ ".ssl.keyfile", "./priv/ssl/yaws-key.pem") },
				{ certfile, ewok:config(Prefix ++ ".ssl.certfile", "./priv/ssl/yaws-cert.pem") }
				%{ cacertfile, ewok:config("ewok.http.ssl.cacertfile", "./priv/ssl/cacerts.pem") } 
			];
		false -> [ %% add in options that are not supported for ssl sockets
				{ recbuf, ewok:config(Prefix ++ ".tcp.socket.recbuf", 8192) }
			]
		end,
	lists:append(SocketOpts, ExtraOpts).

%
connections(Name) ->
	gen_server2:call(Name, connections).

%%
start_link(Name, Opts) when is_atom(Name), is_list(Opts) ->
	gen_server2:start_link({local, Name}, ?MODULE, [{name, Name}|Opts], []).
%
stop(Name) when is_atom(Name) -> gen_server2:cast(Name, stop);
stop(Pid) when is_pid(Pid) -> gen_server2:cast(Pid, stop).

%%
%% gen_server
%%
init(Opts) ->
    process_flag(trap_exit, true),	
	Port = proplists:get_value(port, Opts, 0),
	SocketOpts = proplists:get_value(socket_opts, Opts),
	Transport = proplists:get_value(transport, Opts, gen_tcp),
	%% IMPL: 'old ssl' impl should be seeded -- this can probably be removed...
	case Transport of
	ssl -> ssl:seed(ewok_identity:key());
	_ -> ok
	end,
	
	%?TTY("Listen port ~p (~p)~nOpts: ~p~n", [Port, Handler, SocketOpts]), 
	case Transport:listen(Port, SocketOpts) of
	{ok, ServerSocket} ->
		%% Check port...
		{ok, {_, Port}} = 
			case Transport of
			gen_tcp -> inet:sockname(ServerSocket);
			ssl -> ssl:sockname(ServerSocket)
			end,
		State = #state{
			%% NOTE: extract IP from SocketOpts, not main Opts	
			ip = proplists:get_value(ip, SocketOpts, {0, 0, 0, 0}), 
			name = proplists:get_value(name, Opts), 
			protocol = proplists:get_value(protocol, Opts),		
			port = Port,
			socket = {Transport, ServerSocket},
			max = proplists:get_value(max_connections, Opts),
			%% TODO: pre-check the handler arity
			handler = proplists:get_value(handler, Opts)
		},
		{ok, listen(State)};
	{error, Reason} ->
		{stop, Reason}
    end.
	
listen(S = #state{socket=ServerSocket, handler=Handler, count=Count, max=Max}) ->
	case Count < Max of
	true ->
		%% ?TTY("Spawning accept~n", []),
		Pid = proc_lib:spawn_link(?MODULE, accept, [{self(), ServerSocket, Handler}]),
		S#state{pid=Pid};
	false ->
		ewok_log:info([{application, ?MODULE}, "Not accepting new connections", S]),
		S#state{pid=null}
	end.

accept({Pid, {gen_tcp, ServerSocket}, Handler}) ->
	%?TTY("ACCEPT: ~p~n", [{Pid, ServerSocket, inet:getstat(ServerSocket)}]),
	try begin
		{ok, Socket} = gen_tcp:accept(ServerSocket),
		gen_server2:cast(Pid, {accepted, self()}),
		Handler({gen_tcp, Socket})
	end catch
	exit: normal ->
		exit(normal);
	Error: Reason ->
%		Reason = lists:flatten(io_lib:format("~p", [Other])), 
		ewok_log:error([{application, ewok}, "Accept failed", {Error, Reason}]),
		exit({error, accept_failed})
    end;

accept({Pid, {ssl, ServerSocket}, Handler}) ->
	%?TTY("SSL ACCEPT: ~p~n", [{Pid, ServerSocket, inet:getstat(ServerSocket)}]),
	try begin
		%?TTY("Calling transport_accept~n", []),
		{ok, Socket} = ssl:transport_accept(ServerSocket),
		%?TTY("Calling ssl_accept~n", []),
		ok = ssl:ssl_accept(Socket),
		gen_server2:cast(Pid, {accepted, self()}),
		%?TTY("Calling handler~n", []),
		Handler({ssl, Socket})
	end catch 
	error:{badmatch, {error, closed}} ->
		exit({error, closed});
	error:Reason ->
		ewok_log:error([{{application, ewok}, "Accept failed", {error, Reason}}]),
		exit({error, accept_failed})
    end.

%
handle_call(connections, _From, State) ->
    {reply, {State#state.count, State#state.max}, State};
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
handle_info(_E ={'EXIT', Pid, normal}, State=#state{pid=Pid}) ->
	% ?TTY("1. ~p~n", [E]),
    {noreply, listen(State)};
handle_info(_E = {'EXIT', Pid, Reason}, State=#state{pid=Pid}) ->
	% ?TTY("2. ~p~n", [E]),
    ewok_log:error({?MODULE, ?LINE, {acceptor_error, Reason}}),
    timer:sleep(100),
    {noreply, listen(State)};
handle_info(_E = {'EXIT', _LoopPid, Reason}, State=#state{pid=Pid, count=Count}) ->
	% ?TTY("3. ~p~n", [E]),
    case Reason of 
	normal -> ok;
	Error -> ewok_log:error({?MODULE, ?LINE, {child_error, Error, Reason}})
    end,
	NewState = State#state{count=Count-1},
    FinalState = 
		case Pid of
		null -> listen(NewState);
		_ -> NewState
		end,
    {noreply, FinalState};
handle_info(Info, State) ->
    ewok_log:info([{info, Info}, {state, State}]),
    {noreply, State}.
%	
code_change(_OldVsn, State, _Extra) ->
    State.
%
terminate(_Reason, #state{socket={Transport, Socket}}) ->
    Transport:close(Socket).

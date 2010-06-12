%% Copyright 2010 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(ewok_socket_srv).

-include("ewok.hrl"). %% debug only
-include("ewok_system.hrl").

-behaviour(gen_server2).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, code_change/3, terminate/2]).

-export([connections/1, start_link/2, stop/1]).
-export([accept/1]).

% internal state record
-record(state, {
	name,
	port, 
	ip={0,0,0,0},
	transport=gen_tcp,
	socket,
	protocol,
	handler,
	pid,
	count=0,
	max=infinity
}).

%
connections(Name) ->
	gen_server2:call(Name, connections).

%%
start_link(Name, Opts) when is_atom(Name), is_list(Opts) ->
	gen_server2:start_link({local, Name}, ?MODULE, [{name, Name}|Opts], []).
%
stop(Name) when is_atom(Name); is_pid(Name) -> 
	gen_server2:cast(Name, stop).

%%
%% gen_server
%%

%%
init(Opts) ->
    process_flag(trap_exit, true),	
	Port = proplists:get_value(port, Opts, 0),
	Transport = proplists:get_value(transport, Opts, gen_tcp),
	SocketOpts = proplists:get_value(socket_opts, Opts, []),
	
	%?TTY("Listen port ~p Opts: ~p~n", [Port, SocketOpts]), 
	case ewok_socket:listen(Transport, Port, SocketOpts) of
	{ok, ServerSocket} ->
		%% Check port...
		{ok, {_, Port}} = ewok_socket:sockname(Transport, ServerSocket),
		State = #state{
			%% NOTE: extract IP from SocketOpts, not main Opts	
			ip = proplists:get_value(ip, SocketOpts, {0, 0, 0, 0}), 
			name = proplists:get_value(name, Opts), 
			protocol = proplists:get_value(protocol, Opts),		
			port = Port,
			transport = Transport,
			socket = ServerSocket,
			max = proplists:get_value(max_connections, Opts),
			%% TODO: pre-check the handler arity
			handler = proplists:get_value(handler, Opts)
		},
		{ok, listen(State)};
	{error, Reason} ->
		{stop, Reason}
    end.
	
listen(S = #state{count=Count, max=Max}) ->
	case Count < Max of
	true ->
		Pid = proc_lib:spawn_link(?MODULE, accept, [{self(), S#state.transport, S#state.socket, S#state.handler}]),
		S#state{pid=Pid};
	false ->
		ewok_log:info([{application, ?MODULE}, "Not accepting new connections", S]),
		S#state{pid=undefined}
	end.

accept({Pid, Transport, ServerSocket, Handler}) ->
	%io:format("ACCEPT: ~p~n", [{Pid, ServerSocket}]),
	try begin
		{ok, Socket} = ewok_socket:accept({Transport, ServerSocket}),
		gen_server2:cast(Pid, {accepted, self()}),
		Handler({Transport, Socket})
	end catch
	exit : normal ->
		exit(normal);
	Error: Reason ->
		ewok_log:error([{application, ewok}, "Accept failed", {Error, Reason}]),
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
handle_info(_E = {'EXIT', Pid, normal}, State=#state{pid=Pid}) ->
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
	NewState = State#state{count=Count - 1},
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
terminate(_Reason, #state{transport=Transport, socket=Socket}) ->
    Transport:close(Socket).

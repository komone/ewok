%% Copyright 2009-2010 Steve Davis <steve@simulacity.com>
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

-module(ewok_inet).
-include("ewok.hrl").
-include("ewok_system.hrl").

%-behaviour(ewok_service).
-export([start_link/1, stop/1]).
-export([behaviour_info/1]).
-export([make_name/1]).

-behaviour(gen_server2).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, code_change/3, terminate/2]).

-export([connections/1]).

-record(state, {inet, socket, pid, connections = 0}).

%%
behaviour_info(callbacks) -> 
	[{start, 1}, {init, 2}, {terminate, 3}];
behaviour_info(_) -> 
	undefined.

%%
start_link(Inet = #ewok_inet{id = Name}) ->
	gen_server2:start_link({local, Name}, ?MODULE, Inet, []).

%%
stop(Port) when is_integer(Port) ->
	stop(make_name(Port));
stop(Name) when is_atom(Name) ->
	gen_server2:cast(Name, stop).


%% TODO: move to ewok_inet_sup?
make_name(Port) ->
	String = lists:flatten([atom_to_list(?MODULE), $_, integer_to_list(Port)]),
	list_to_atom(String).
%%
connections(Port) ->
	gen_server2:call(make_name(Port), connections).

%% callbacks: gen_server
init(Inet = #ewok_inet{transport = Transport, port = Port}) ->
	case catch ewok_socket:listen(Transport, Port) of
	{ok, ServerSocket} ->
		?TTY({Inet#ewok_inet.protocol, Transport, Port}),
		{ok, Pid} = accept(Transport, ServerSocket, Inet),
		State = #state{pid = Pid, inet = Inet, socket = ServerSocket},
		{ok, State};
	Error ->
		?TTY(Error),
		Error
	end.

accept(udp, _ServerSocket, _Inet) ->
	{ok, self()};
accept(tcp, ServerSocket, Inet) ->
	ewok_connection:start_link(ServerSocket, Inet);
accept(ssl, ServerSocket, Inet) ->
	ewok_connection:start_link(ServerSocket, Inet).
%% sctp...?

%%
handle_call(connections, _From, State = #state{connections = Connections}) ->
	{reply, {ok, Connections}, State};
handle_call(Message, _From, State) ->
	?TTY({call, Message}),
	{reply, ok, State}.
%%
handle_cast({connected, Pid}, State = #state{pid = Pid}) ->
	#state{inet = Inet, socket = ServerSocket, connections = Connections} = State,
	{ok, NextPid} = ewok_connection:start_link(ServerSocket, Inet),
	{noreply, State#state{pid = NextPid, connections = Connections + 1}};
%
handle_cast(stop, State) ->
	?TTY({cast, stop}),   
	{stop, normal, State};
%	
handle_cast(Message, State) ->
	?TTY({cast, Message}),
	{noreply, State}.

%%	
handle_info(Message = {udp, _, _, _, _}, State = #state{inet = Inet}) ->
	ok = handle_message(Message, Inet),
    {noreply, State};
handle_info(Message, State) ->
	?TTY({info, Message}),
    {noreply, State}.

%%
code_change(_OldVsn, State, _Extra) ->
    State.
%%
terminate(_Reason, _State) ->
	ok.
	
handle_message({udp, Socket, Address, Port, Packet}, #ewok_inet{handler = Handler, codec = Codec}) ->
	{ok, Message} = Codec:decode(Packet),
	try Handler:init(Message, {Address, Port}) of
	{reply, Reply, _, _} ->
		{ok, Data} = Codec:encode(Reply),
%		?TTY({send, Socket, Address, Port, Data}),
		ok = ewok_socket:send({gen_udp, Socket}, Address, Port, Data);
	{noreply, _, _} ->
		ok
	catch E:R ->
		{handler_error, {E, R}}
	end.

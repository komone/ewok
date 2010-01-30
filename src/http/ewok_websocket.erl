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

-module(ewok_websocket).
-vsn("1.0.0").
-author('steve@simulacity.com').

-include("ewok.hrl").

-export([start/1, stop/1]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(USAGE, "valid commands are 'start' and 'stop'").

-record(state, {socket, timer}).

start(Socket) ->
    gen_server:start_link(?MODULE, [Socket], []).
	
stop(Pid) ->
    gen_server:cast(Pid, stop).

init([Socket]) ->
	ewok_socket:setopts(Socket, [binary, {packet,0}, 
		{active, true}, {reuseaddr,true}]),
%	?TTY("WEBSOCKET ~p~n", [Socket]),
%	ewok_socket:send(Socket, [<<0>>, <<"websocket server started">>, <<255>>]),
    ewok_log:info("websocket server started"),
    {ok, #state{socket=Socket}}.

handle_call(Message, _From, State) ->
	ewok:log(["websocket call", Message]),
    {reply, ok, State}.

handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(Message, State) ->
	ewok_log:warn(["websocket cast", Message]),
    {noreply, State}.

handle_info({tcp, _Socket, Data}, State) ->
	ewok_log:info([{"browser: ", unframe(Data)}]),
	NewState = 
		case unframe(Data) of
		"start" -> 
			case State#state.timer of
			Ref when is_reference(Ref) ->
				send(State#state.socket, "already started!"),
				State;
			_ ->
				send(State#state.socket, ewok_util:timestamp()),
				Ref = erlang:start_timer(1000, self(), update),
				State#state{timer=Ref}
			end;
		"stop" ->
			case State#state.timer of
			Ref when is_reference(Ref) -> 
				erlang:cancel_timer(Ref),
				State#state{timer=undefined};
			_ -> 
				send(State#state.socket, "already stopped!"),
				State
			end;
		_ -> 
			send(State#state.socket, ?USAGE),
			State
		end,
    {noreply, NewState};
handle_info({tcp_closed, _Socket}, State) ->
	cleanup(State),
	ewok_log:debug("websocket closed"),
	{stop, normal, State};
handle_info({tcp_error, _Socket, Reason}, State) ->
	ewok_log:warn([{"websocket error", Reason}]),
	{stop, normal, State};
handle_info({timeout, _Ref, update}, State) ->
	send(State#state.socket, ewok_util:timestamp()),
	Ref = erlang:start_timer(1000, self(), update),
    {noreply, State#state{timer=Ref}};	
handle_info(Message, State) ->
	ewok_log:warn(["websocket info", Message]),
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, State) ->
	cleanup(State),
    ewok_log:info("websocket server stopped"),
    ok.

send(Socket, Text) ->
	ewok_socket:send(Socket, frame(Text)).

cleanup(State) ->
	case State#state.timer of
	Ref when is_reference(Ref) -> 
		erlang:cancel_timer(Ref);
	_ ->
		ok
	end.
	

unframe(Data) when is_binary(Data) ->
	unframe(binary_to_list(Data));
unframe([255]) -> 
	[];
unframe([0|T]) -> 
	unframe(T); 
unframe([H|T]) -> 
	[H|unframe(T)].

frame(Data) when is_list(Data) ->
	frame(list_to_binary(Data));
frame(Data) when is_binary(Data) ->
	[<<0>>, Data, <<16#ff>>].
	

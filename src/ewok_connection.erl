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

-module(ewok_connection).
-include("ewok.hrl").
-include("ewok_system.hrl").

-export([start_link/2]).

-behaviour(gen_server2).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, code_change/3, terminate/2]).

-record(state, {socket, timeout, handler, codec, state, data}).

start_link(ServerSocket, Inet) ->
	{ok, Pid} = gen_server2:start_link(ewok_connection, Inet, []),
	ok = gen_server2:cast(Pid, {accept, ServerSocket, self()}),
	{ok, Pid}.

%% callbacks: gen_server 
init(#ewok_inet{handler = Handler, codec = Codec, timeout = Timeout}) ->
	process_flag(trap_exit, true),
	{ok, #state{handler = Handler, codec = Codec, timeout = timeout(Timeout)}}.	


%
handle_call(Message, _From, State) ->
	?TTY({call, Message}),
	{reply, ok, State}.
%
handle_cast({accept, ServerSocket, Pid}, State) ->
	case catch ewok_socket:accept(ServerSocket) of
	{ok, Socket} ->
%		{ssl, RawSocket} = Socket,
%		{ok, Cert} = ssl:peercert(RawSocket),
%		{ok, DecodedCert} = public_key:pkix_decode_cert(Cert, plain),
%		?TTY({cert, size(DecodedCert)}),
		ok = gen_server2:cast(Pid, {connected, self()}),
		handle_connection(State#state{socket = Socket});
	{error, closed} ->
		{stop, shutdown, State};
	Error ->
		?TTY({possible_ssl_error, Error}),
		{stop, Error, State}
	end;
handle_cast(stop, State) ->
	?TTY({cast, stop}),   
	{stop, normal, State};
	
handle_cast(Message, State) ->
	?TTY({cast, Message}),
	{noreply, State}.
%
handle_info({tcp, _RawSocket, Message}, State) ->
%	?TTY({tcp, Socket}),
	handle_request(Message, State);
handle_info({ssl, _RawSocket, Message}, State) ->
	handle_request(Message, State);
handle_info({ssl_closed, _RawSocket}, State = #state{socket = Socket}) ->
	ok = ewok_socket:close(Socket),
	?TTY({ssl_closed, close_socket}),
	{stop, normal, State};
handle_info({tcp_closed, _RawSocket}, State = #state{socket = Socket}) ->
	ok = ewok_socket:close(Socket),
	?TTY({tcp_closed, close_socket}),
	{stop, normal, State};
handle_info(timeout, State = #state{socket = Socket}) ->
	ok = ewok_socket:close(Socket),
	?TTY({timeout, State#state.timeout}),
	{stop, normal, State};
handle_info(Message, State) ->
	?TTY({info, Message}),
    {noreply, State}.
%	
code_change(_OldVsn, State, _Extra) ->
    State.
%
terminate(_Reason, _State) ->
	ok.

timeout(T) when is_integer(T), T >= 0 ->
	T * 1000;
timeout(T = infinity) ->
	T.
%%
handle_connection(State = #state{socket = Socket, timeout = Timeout, handler = Handler}) ->
	{ok, Remote} = ewok_socket:peername(Socket),
	case catch Handler:init([], Remote) of
	{noreply, NextState, StateData} ->
		%?TTY({Handler, NextState, StateData}),
		{noreply, State#state{state = NextState, data = StateData}, Timeout};
	{reply, Response, NextState, StateData} ->
		respond(Response, State#state{state = NextState, data = StateData});
	Error ->
		?TTY({Handler, Error}),
		{stop, Error, State}
	end.
%%
handle_request(Message, State) ->
	#state{handler = Handler, codec = Codec, state = Callback, data = StateData} = State,
	{ok, Request} = Codec:decode(Message),
	{reply, Response, NextState, NewStateData} = Handler:Callback(Request, StateData),
	respond(Response, State#state{state = NextState, data = NewStateData}).

%%
respond(Response, State) ->
	#state{socket = Socket, timeout = Timeout, codec = Codec, state = Callback} = State,
	{ok, Data} = Codec:encode(Response),
	ok = ewok_socket:send(Socket, Data),
	case Callback of
	terminate ->
		{stop, normal, State};
	_ ->
		{noreply, State, Timeout}
	end.

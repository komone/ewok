%% Copyright 2009-2010 Steve Davis <steve@simulacity.com>
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

-module(ewok_scm_srv).
-name("Ewok Source Code Management Service").

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

%% API
-export([service/2]).
-export([format_log/3]).

-compile(export_all).
-define(SCM_PORT, 9418).
%%
start_link(_Args) ->
	try begin
		ewok_log:message(service, ?MODULE),

		%% DS = ewok_data_srv:connect(default)
		Transport = gen_tcp, %% TEMP
		Port = ewok:config({ewok, scm, port}, ?SCM_PORT),
		SocketOpts = ewok_socket:configure(Transport, {ewok, scm}),
		MaxConnections = ewok:config("ewok.scm.tcp.max_connections", infinity),
		Timeout = ewok:config({ewok, scm, request_timeout}, 120) * 1000,
		
		Handler = fun(X) -> ?MODULE:service(X, Timeout) end,
		
		Configuration = [
			{name, ?MODULE},
			{transport, Transport}, %% defines use of gen_tcp or ssl module
			{port, Port},
			{protocol, git},
			{max_connections, MaxConnections},
			{socket_opts, SocketOpts},
			{handler, Handler}
		],
		%?TTY("CONFIG:~n~p~n", [Configuration]),
		ewok_log:message(configuration, {?MODULE, Configuration}),
		%% Starts a TCP Server for SMTP
		{ok, Pid} = ewok_socket_srv:start_link(?MODULE, Configuration),
		ewok_log:add_log(git),
		{ok, Pid}
	end catch
	Error:Reason -> {Error, Reason}
	end.

%%	
stop() -> 
	ewok_socket_srv:stop(?MODULE),
	ewok_log:remove_log(git).

%% Callback from ewok_socket_srv, handing over a client connection
service(Socket, Timeout) ->
	{ok, Session} = ewok_scm_session:create(),
	?TTY({"FSM", Session}),
	handle_request(Socket, Session, Timeout, []).

%
handle_request(Socket, Session, Timeout, Acc) ->
	case ewok_socket:recv(Socket, 0, Timeout) of
	{ok, Packet} ->
		?TTY({"PACKET", Packet}),
		Response = ewok_scm_session:request(Session, {request, Packet}),
		?TTY({"RESPONSE", Response}),
		case Response of
		close ->
			ewok_socket:close(Socket);
		continue -> 
			handle_request(Socket, Session, Timeout, Acc);
		_ when is_binary(Response) -> 
			case ewok_socket:send(Socket, Response) of
			ok -> 
				handle_request(Socket, Session, Timeout, Acc);
			_ -> 
				exit(normal)
			end
		end;
	{error, Reason} ->
		ewok_scm_session:request(Session, close),
		?TTY({"ERROR", Reason}),
		exit(normal)
	end.


%%
%% INTERNAL
%%

%% TODO: Move this later on. To...?
format_log(_Session, _StatusCode, _BytesSent) ->
	ok.

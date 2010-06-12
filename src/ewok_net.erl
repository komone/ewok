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
-module(ewok_net).
-include("ewok.hrl").
-include("ewok_system.hrl").

-export([start_link/2, stop/1, service/2]).
-export([behaviour_info/1]).

behaviour_info(callbacks) -> [
	{init, 1},
	{request, 2}
];

behaviour_info(_) ->
    undefined. 

start_link(Module, Opts) -> 
	try begin
		ewok_log:info({service, Module}),
		%% DS = ewok_data_srv:connect(default)
		Protocol = properties:get_value(protocol, Opts),
		Port = ewok_config:get_value(port, Opts),
		Transport = ewok_config:get_value(transport, Opts, gen_tcp), %%
		
		SocketOpts = ewok_socket:configure(Transport, {ewok, Protocol}),
		MaxConnections = ewok_config:get_value({ewok, Protocol, max_connections}, infinity),
		Timeout = ewok_config:get_value({ewok, Protocol, timeout}, 30) * 1000,
		
		Handler = fun(X) -> ?MODULE:service(X, Timeout) end,
		
		Configuration = [
			{name, Module},
			{protocol, Protocol},
			{port, Port},
			{transport, Transport}, %% defines use of gen_tcp or ssl module
			{max_connections, MaxConnections},
			{socket_opts, SocketOpts},
			{handler, Handler}
		],
		
		%% ?TTY("CONFIG:~n~p~n", [Configuration]),
		ewok_log:message(configuration, {Module, Configuration}),
		%% Starts a TCP Server for UTP
		{ok, Pid} = ewok_socket_srv:start_link(Module, Configuration),
		ewok_log:add_log(Protocol),
		{ok, Pid}
	end catch
	Error:Reason -> 
		{Error, Reason}
	end.

stop(Name) -> 
	ewok_socket_srv:stop(Name),
	ewok_log:remove_log(ldap).

%% Callback from ewok_socket_srv, handing over a client connection
service(Socket, Timeout) ->
	{ok, Session} = ewok_ldap_session:create(),
	ewok_log:message(ldap, {session, [{pid, Session}, {peer, ewok_socket:peername(Socket)}]}),
	?TTY({session, Session}),
	service(Socket, Session, 1, Timeout).
	
%%
service(Socket, Session, Sequence, Timeout) ->
	try begin
		{ok, Packet} = ewok_socket:recv(Socket, 0, Timeout),
		Request = ldap:decode(Packet),
		?TTY({request, Request}),
		Response = ewok_ldap_session:request(Session, Request),
		?TTY({response, Response}),
		Data = ldap:encode(Response),
		ok = ewok_socket:send(Socket, Data),
		service(Socket, Session, Sequence + 1, Timeout)
	end catch
	Error:Reason ->
		ewok_ldap_session:close(Session),
		?TTY({Error, Reason}),
		exit(normal)		
	end.

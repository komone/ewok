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

-module(ewok_umtp).
-vsn("1.0.0").
-author('steve@simulacity.com').

-include("ewok.hrl").
-include("umtp.hrl").

%% Universal Message Transfer Protocol
%% Suggested IANA Port 30
%% Suggested Well-known ports 3300-3301
%% Stateless

-behaviour(ewok_service).
-export([start_link/0, stop/0, service_info/0]).
-export([service/2, sendmail/1]).

%-record(mail, {from, to, timestamp, body}).

start_link() -> 
	try begin
		ewok_log:log(default, service, {?MODULE, service_info()}),
		%% DS = ewok_data_srv:connect(default)
		true = is_pid(whereis(ewok_cache_srv)),
		Transport = gen_tcp, %% TEMP
		Port = ewok:config({ewok, umtp, port}, 30),
		SocketOpts = ewok_socket:configure(Transport, "ewok.umtp"),
		MaxConnections = ewok:config("ewok.umtp.tcp.max_connections", infinity),
		Timeout = ewok:config({ewok, umtp, timeout}, 10) * 1000,
		
		Handler = fun(X) -> ?MODULE:service(X, Timeout) end,
		
		Configuration = [
			{name, ?MODULE},
			{transport, Transport}, %% defines use of gen_tcp or ssl module
			{port, Port},
			{protocol, umtp},
			{max_connections, MaxConnections},
			{socket_opts, SocketOpts},
			{handler, Handler}
		],
		%% ?TTY("CONFIG:~n~p~n", [Configuration]),
		ewok_log:log(default, configuration, {?MODULE, Configuration}),
		%% Starts a TCP Server for UTP
		{ok, Pid} = ewok_socket_srv:start_link(?MODULE, Configuration),
		ewok_log:add_log(mail),
		{ok, Pid}
	end catch
	Error:Reason -> {Error, Reason}
	end.

stop() -> 
	ewok_socket_srv:stop(?MODULE),
	ewok_log:remove_log(mail).

service_info() -> [
	{name, "Ewok UMTP Service"},
	{version, {1, 0, 0}},
	{comment, ""},
	{depends, [ewok_cache_srv]}
].

%% Callback from ewok_socket_srv, handing over a client connection
service({Transport, Socket}, Timeout) ->
	case Transport:recv(Socket, 0, Timeout) of
	{ok, Packet} ->
		try begin
			Message = ewok_ubf:decode(Packet),
			?TTY("~p~n", [Message]),
			reply({Transport, Socket}, {reply, ok})
		end catch 
			Error:Reason ->
				reply({Transport, Socket}, {Error, Reason})
		end;
	{error, Reason} ->
		?TTY("ERROR: ~p~n", [Reason])
	end.

reply({Transport, Socket}, Reply) ->
	case Transport:send(Socket, ewok_ubf:encode(Reply)) of
	ok -> ok;
	_ -> exit(normal)
	end.

sendmail(Mail = #mail{}) ->
	{ok, Host} = inet:gethostname(),
	{ok, Socket} = gen_tcp:connect(Host, 30, [{active, false}]),
	gen_tcp:send(Socket, ewok_ubf:encode(Mail)),
	print(Socket),
	gen_tcp:close(Socket).

print(Socket) ->
	{ok, Packet} = gen_tcp:recv(Socket, 0),
	io:format("~p~n", [Packet]).

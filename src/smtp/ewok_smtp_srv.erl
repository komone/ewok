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

-module(ewok_smtp_srv).
-name("Ewok SMTP Service").

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("email.hrl").

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

%% API
-export([service/2]).
-export([format_log/3]).

-compile(export_all).

%%
start_link(_Args) ->
	try begin
		ewok_log:message(service, ?MODULE),

		%% DS = ewok_data_srv:connect(default)
		Transport = gen_tcp, %% TEMP
		Port = ewok:config({ewok, smtp, port}, 2525),
		SocketOpts = ewok_socket:configure(Transport, {ewok, smtp}),
		MaxConnections = ewok:config("ewok.smtp.tcp.max_connections", infinity),
		Timeout = ewok:config({ewok, smtp, request_timeout}, 30) * 1000,
		
		Handler = fun(X) -> ?MODULE:service(X, Timeout) end,
		
		Configuration = [
			{name, ?MODULE},
			{transport, Transport}, %% defines use of gen_tcp or ssl module
			{port, Port},
			{protocol, smtp},
			{max_connections, MaxConnections},
			{socket_opts, SocketOpts},
			{handler, Handler}
		],
		%?TTY("CONFIG:~n~p~n", [Configuration]),
		ewok_log:message(configuration, {?MODULE, Configuration}),
		%% Starts a TCP Server for SMTP
		{ok, Pid} = ewok_socket_srv:start_link(?MODULE, Configuration),
		ewok_log:add_log(mail),
		{ok, Pid}
	end catch
	Error:Reason -> {Error, Reason}
	end.

%%	
stop() -> 
	ewok_socket_srv:stop(?MODULE),
	ewok_log:remove_log(mail).

%% Callback from ewok_socket_srv, handing over a client connection
service(Socket, Timeout) ->
	{ok, Session, Reply} = ewok_smtpd_session:create(),
	?TTY({"FSM", Session}),
	send_reply(Socket, Reply),
	read_request(Socket, Session, Timeout, []).

%
read_request(Socket, Session, Timeout, Acc) ->
	case ewok_socket:recv(Socket, 0, Timeout) of
	{ok, Packet} ->
		Parts = ewok_text:split(Packet, <<"(\r\n)">>),
		case process_request(Socket, Session, Parts, Acc) of
		{ok, Acc1} -> 
			read_request(Socket, Session, Timeout, Acc1);
		close ->
			ewok_smtpd_session:request(Session, close),
			ewok_socket:close(Socket)
		end;
	{error, Reason} ->
		ewok_smtpd_session:request(Session, close),
		?TTY({"ERROR", Reason})
	end.
%
process_request(Connection, Session, [?CRLF|T], Acc) ->
	case handle_request(Session, list_to_binary(lists:reverse(Acc))) of
	closing_channel ->
		send_reply(Connection, closing_channel),
		close;
	continue -> 
		process_request(Connection, Session, T, []);
	Response -> 
		send_reply(Connection, Response),
		process_request(Connection, Session, T, [])
	end;
process_request(Connection, Session, [H|T], Acc) ->
	process_request(Connection, Session, T, [H|Acc]);
process_request(_Connection, _Session, [], Acc) ->
	{ok, Acc}.

%
handle_request(Session, Request) ->
	ewok_log:message(mail, session, Request),
	[Verb|Args] = ewok_text:split(Request, <<$ >>, 2),
	R = case ewok_smtp:command(ewok_text:to_upper(Verb)) of
		undefined -> 
			{undefined, Request};
		Command -> 
			{Command, Args}
		end,
	ewok_smtpd_session:request(Session, R).

%%
send_reply(Socket, Reply) ->
	Code = ewok_smtp:code(Reply),
	Data = [integer_to_list(Code), ?SP, ewok_smtp:status_message(Code), ?CRLF],
    case ewok_socket:send(Socket, Data) of
	ok -> 
		ok;
	_ -> 
		exit(normal) %% leaving a file open?
    end.

%%
%% INTERNAL
%%

%% TODO: Move this later on. To...?
format_log(_Session, _StatusCode, _BytesSent) ->
	ok.

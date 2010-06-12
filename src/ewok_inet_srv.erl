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

-module(ewok_inet_srv).
-name("Ewok INET Service").

-include("ewok.hrl").
-include("ewok_system.hrl").

-export([start_link/4, stop/0]).

%% API
-export([service/3]).
-export([format_mail_log/3]).

-compile(export_all).

-define(CRLF, <<"\r\n">>).
-define(SP, <<" ">>).

%-record(ewok_inet, {name, handler, codec, port, transport, timeout, max_connections}).

%%
start_link(Protocol, ProtocolHandler, _Codec, Opts) ->
	try begin
		ewok_log:message(service, ProtocolHandler),

		%% DS = ewok_data_srv:connect(default)
		Transport = properties:get_value(transport, Opts, gen_tcp),
		Port = properties:get_value(port, Opts, 0),
		
		SocketOpts = ewok_socket:configure(Transport, {ewok, Protocol}),
		MaxConnections = properties:get_value(max_connections, Opts, infinity),
		Timeout = properties:get_value(timeout, Opts, 30) * 1000,
		
		Handler = fun(X) -> ?MODULE:service(X, ProtocolHandler, Timeout) end,
		
		Configuration = [
			{name, ProtocolHandler},
			{transport, Transport}, %% defines use of gen_tcp or ssl module
			{port, Port},
			{protocol, Protocol},
			{max_connections, MaxConnections},
			{socket_opts, SocketOpts},
			{handler, Handler}
		],
		%?TTY("CONFIG:~n~p~n", [Configuration]),
		ewok_log:message(configuration, {ProtocolHandler, Configuration}),
		%% Starts a TCP Server for SMTP
		{ok, Pid} = ewok_socket_srv:start_link(ProtocolHandler, Configuration),
		ewok_log:add_log(Protocol),
		{ok, Pid}
	end catch
	Error:Reason -> {Error, Reason}
	end.

%%	
stop() -> 
	ewok_socket_srv:stop(?MODULE),
	ewok_log:remove_log(mail).

%% Callback from ewok_socket_srv, handing over a client connection
service(Socket, ProtocolHandler, Timeout) ->
	{ok, Session, Reply} = ProtocolHandler:create(),
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
format_mail_log(_Session, _StatusCode, _BytesSent) ->
	ok.

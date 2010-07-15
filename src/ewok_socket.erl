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

-module(ewok_socket).
-include("ewok.hrl").

-export([connect/4, getopts/1, setopts/2, controlling_process/2, sockname/1,
	peername/1, listen/2, accept/1, upgrade/1, send/2, send/4, recv/3, close/1]).

% internalize
-export([transport_impl/1]).

-define(READ_SIZE, 8192).

%
transport_impl(udp) -> gen_udp;
transport_impl(tcp) -> gen_tcp;
transport_impl(ssl) -> ssl.

%%
connect(Transport, Host, Port, Options) when is_binary(Host) ->
	connect(Transport, binary_to_list(Host), Port, Options);
connect(Transport, Host, Port, Options) when is_binary(Port) ->
	Number = list_to_integer(binary_to_list(Port)),
	connect(Transport, Host, Number, Options);
connect(gen_udp, _Host, Port, Options) ->
	gen_udp:open(Port, Options);
connect(ssl, Host, Port, _Options) ->
    ssl:connect(Host, Port, [binary, {active, false}, {ssl_imp, old}]); %Options).
connect(Transport, Host, Port, Options) ->
    Transport:connect(Host, Port, Options).


getopts(udp) -> 
	[binary, {ip, {0, 0, 0, 0}}, {active, true}];
getopts(tcp) -> 
	[binary, {ip, {0, 0, 0, 0}}, {active, true}, {packet, 0}, 
	{nodelay, true}, {reuseaddr, true}, {recbuf, 8192}, {backlog, 5}];
getopts(ssl) ->
	[binary, {ip, {0, 0, 0, 0}}, {active, true}, {packet, 0}, 
	{ssl_imp, old}, {verify, 0}, {depth, 1}, 
	{keyfile, "./priv/data/auth/yaws-key.pem"},
	{certfile, "./priv/data/auth/yaws-cert.pem"}].
%	{cacertfile, "./priv/data/auth/cacert.pem"}].
%	{password, undefined}

%%
setopts({gen_udp, Socket}, Opts) ->
	inet:setopts(Socket, Opts);	
setopts({gen_tcp, Socket}, Opts) ->
	inet:setopts(Socket, Opts);
setopts({ssl, Socket}, Opts) ->
	ssl:setopts(Socket, Opts).
%%
controlling_process({Transport, Socket}, Pid) when is_pid(Pid) ->
	Transport:controlling_process(Socket, Pid).

%%
sockname({gen_udp, Socket}) ->
	inet:sockname(Socket);
sockname({gen_tcp, Socket}) ->
	inet:sockname(Socket);
sockname({ssl, Socket}) ->
	ssl:sockname(Socket).
	
%%
peername({gen_udp, _Socket}) ->
	undefined;
peername({gen_tcp, Socket}) ->
	inet:peername(Socket);
peername({ssl, Socket}) ->
	ssl:peername(Socket).
	
%%
listen(Transport, Port) ->
	listen(Transport, Port, getopts(Transport)).
%
listen(udp, Port, Opts) ->
	{ok, ServerSocket} = gen_udp:open(Port, Opts),
	{ok, {gen_udp, ServerSocket}};
listen(tcp, Port, Opts) ->
	{ok, ServerSocket} = gen_tcp:listen(Port, Opts),
	{ok, {gen_tcp, ServerSocket}};
listen(ssl, Port, Opts) ->
	ssl:start(),
	%% IMPL: 'old ssl' impl should be seeded -- this can probably be removed...
	ssl:seed(ewok_identity:key()),
	{ok, Socket} = ssl:listen(Port, Opts),
	{ok, {ssl, Socket}}.

%%	
accept({gen_tcp, ServerSocket}) ->
	case gen_tcp:accept(ServerSocket, infinity) of
	{ok, Socket} ->
		{ok, {gen_tcp, Socket}};
	Error ->
		Error
	end;
accept({ssl, ServerSocket}) ->
	?TTY(<<"Calling transport_accept">>),
	case ssl:transport_accept(ServerSocket, infinity) of
	{ok, Socket} ->
		?TTY(<<"Calling ssl_accept">>),
		ok = ssl:ssl_accept(Socket),
		{ok, {ssl, Socket}};
	Error ->
		Error
	end.

upgrade({gen_tcp, Socket}) ->
	ssl:start(),
	ok = inet:setopts(Socket, [{active, false}]),
	{ok, NewSocket} = ssl:ssl_accept(Socket, getopts(ssl)),
	ok = ssl:setopts(NewSocket, [{active, true}]),
	{ok, {ssl, NewSocket}}.

%%
recv({Transport, Socket}, Bytes, Timeout) ->
	Transport:recv(Socket, Bytes, Timeout).
	
send({Transport, Socket}, Address, Port, Data) ->
	Transport:send(Socket, Address, Port, Data).
	
%%
send(Socket, Data) when is_binary(Data); is_list(Data) ->
	ok = send_data(Socket, Data);
send(Socket, {file, F}) ->
	{ok, Fd} = file:open(F, [raw, binary]),
	ok = send_file(Socket, Fd).

%%
send_file(Socket, Fd) ->
    case file:read(Fd, ?READ_SIZE) of
	{ok, Data} ->
		ok = send_data(Socket, Data),
		send_file(Socket, Fd);
	eof ->
		file:close(Fd),
		ok
    end.
%%
send_data({Transport, Socket}, Data) ->
    case Transport:send(Socket, Data) of
	ok -> ok;
	_ -> exit(normal) %% leaving a file open?
    end.

%%
close({Transport, Socket}) ->
	Transport:close(Socket).


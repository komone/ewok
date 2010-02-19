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

-module(ewok_socket).
-vsn("1.0.0").
-author('steve@simulacity.com').

-include("ewok.hrl").

-export([configure/2, connect/4, setopts/2, controlling_process/2, sockname/2,
	peername/1, listen/3, accept/1, send/2, recv/3, close/1]).

-define(READ_SIZE, 8192).

%% IMPL: note that 'Transport' is the actual **module name**
configure(gen_tcp, Prefix) -> [ 
	ewok_config:get_value(merge(Prefix, {tcp, socket, mode}), binary),
	{ ip, ewok_config:get_value({ewok, ip}, {0, 0, 0, 0}) }, 
	{ packet, ewok_config:get_value(merge(Prefix, {tcp, socket, packet}), 0) },
	{ backlog, ewok_config:get_value(merge(Prefix, {tcp, socket, backlog}), 0) },
	{ active, ewok_config:get_value(merge(Prefix, {tcp, socket, active}), false) },
	{ nodelay, ewok_config:get_value(merge(Prefix, {tcp, socket, nodelay}), true) },
	{ reuseaddr, ewok_config:get_value(merge(Prefix, {tcp, socket, reuseaddr}), true) },
	{ recbuf, ewok_config:get_value(merge(Prefix, {tcp, socket, recbuf}), 8192) }
];

%
configure(ssl, Prefix) ->
	case ewok_config:get_value(Prefix ++ ".ssl.enabled", false) of
	true -> ssl:start(); %% TODO: not the right place for this
	false -> ok
	end,
	[ ewok_config:get_value(Prefix ++ ".tcp.socket.mode", binary),
	{ ip, ewok_config:get_value({ewok, ip}, {0, 0, 0, 0}) }, 
	{ packet, ewok_config:get_value(Prefix ++ ".tcp.socket.packet", 0) },
	{ backlog, ewok_config:get_value(Prefix ++ ".tcp.socket.backlog", 0) },
	{ active, ewok_config:get_value(Prefix ++ ".tcp.socket.active", false) },
	{ nodelay, ewok_config:get_value(Prefix ++ ".tcp.socket.nodelay", true) },
	{ reuseaddr, ewok_config:get_value(Prefix ++ ".tcp.socket.reuseaddr", true) },
	{ ssl_impl, new }, %% NOTE: ONLY SUPPORT NEW_SSL 
	{ verify, ewok_config:get_value(Prefix ++ ".ssl.verify", 0) },
	{ depth, ewok_config:get_value(Prefix ++ ".ssl.depth", 1) },
	%{ password, ewok_config:get_value("ewok.http.ssl.password", undefined) }, %% TODO: ONLY SET if keyfile is protected
	{ keyfile, ewok_config:get_value(Prefix ++ ".ssl.keyfile", "./priv/ssl/yaws-key.pem") },
	%{ cacertfile, ewok_config:get_value("ewok.http.ssl.cacertfile", "./priv/ssl/cacerts.pem") },
	{ certfile, ewok_config:get_value(Prefix ++ ".ssl.certfile", "./priv/ssl/yaws-cert.pem") }].

% @private
merge(T, T1) ->
	list_to_tuple(lists:append(tuple_to_list(T), tuple_to_list(T1))).


%%
connect(Transport, Host, Port, Options) when is_binary(Host) ->
	connect(Transport, binary_to_list(Host), Port, Options);
connect(Transport, Host, Port, Options) when is_binary(Port) ->
	Number = list_to_integer(binary_to_list(Port)),
	connect(Transport, Host, Number, Options);
connect(Transport, Host, Port, Options) ->
    Transport:connect(Host, Port, Options).

%%
setopts({gen_tcp, Socket}, Opts) ->
	inet:setopts(Socket, Opts);
setopts({ssl, Socket}, Opts) ->
	ssl:setopts(Socket, Opts).
%%
controlling_process({Transport, Socket}, Pid) when is_pid(Pid) ->
	Transport:controlling_process(Socket, Pid).
%%
sockname(gen_tcp, Socket) ->
	inet:sockname(Socket);
sockname(ssl, Socket) ->
	ssl:sockname(Socket).
	
%%
peername({gen_tcp, Socket}) ->
	inet:peername(Socket);
peername({ssl, Socket}) ->
	ssl:peername(Socket).
	
%%
listen(ssl, Port, Opts) ->
	%% IMPL: 'old ssl' impl should be seeded -- this can probably be removed...
	ssl:seed(ewok_identity:key()),
	ssl:listen(Port, Opts);
listen(gen_tcp, Port, Opts) ->
	gen_tcp:listen(Port, Opts).

%%
accept({gen_tcp, Socket}) ->
	{ok, _ClientSocket} = gen_tcp:accept(Socket);
accept({ssl, Socket}) ->
	%?TTY("Calling transport_accept~n", []),
	{ok, ClientSocket} = ssl:transport_accept(Socket),
	%?TTY("Calling ssl_accept~n", []),
	ok = ssl:ssl_accept(ClientSocket),
	{ok, ClientSocket}.

%%
recv({Transport, Socket}, Bytes, Timeout) ->
	Transport:recv(Socket, Bytes, Timeout).
	
%%
send(_, chunked) -> %% placeholder: http only
	ok;
send(Socket, Bin) when is_binary(Bin) ->
	ok = send_data(Socket, Bin);
send(Socket, L) when is_list(L) ->
	ok = send_data(Socket, L);
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

-module(ewok_pop3c).
-include("ewok.hrl").

-compile(export_all).

test() ->
    {ok, RawSocket} = ewok_socket:connect(gen_tcp, <<"localhost">>, 110, [binary, {active, false}, {packet, 0}]),
	Socket = {gen_tcp, RawSocket},
	?TTY(read(Socket)),
	?TTY(request(Socket, <<"AUTH\r\n">>)),
	?TTY(request(Socket, <<"USER steved\r\n">>)),
	?TTY(request(Socket, <<"PASS letmein\r\n">>)),
	?TTY(request(Socket, <<"QUIT\r\n">>)),
	ewok_socket:close(Socket).

read(Socket) ->
	{ok, Bin} = ewok_socket:recv(Socket, 0, 10000),
	Bin.

request(Socket, Data) ->	
	ok = ewok_socket:send(Socket, Data),
	{ok, Bin} = ewok_socket:recv(Socket, 0, 10000),
	Bin.

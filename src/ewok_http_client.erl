-module(ewok_http_client).

-export([connect/2, connect/3]).

connect(Url, Request) ->
	connect(Url, Request, 80).

connect(Url, Request, Port) ->
    {ok, Socket} = gen_tcp:connect(Url, Port, [{active, false}, {packet, 0}]),
    gen_tcp:send(Socket, Request),
    Result = gen_tcp:recv(Socket, 0, 10000),
    gen_tcp:close(Socket),
    Result.
	

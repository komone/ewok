-module(test).
-export([run/0]).

run() ->
    register(regname(), self()),
    P = open_port({spawn, command()}, [{packet, 1}]),
    
    port_close(P),
    unregister(test).
    

regname() -> test.

command() ->
    io_lib:format("./node-test ~d ~a ~d ~a", [502, erlang:get_cookie(), 1, regname()]).

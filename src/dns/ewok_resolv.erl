-module(ewok_resolv).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("dns.hrl").

-compile(export_all).

-define(NS, "192.168.123.254").
-define(LOCAL, "127.0.0.1").

lookup(Name) when ?is_string(Name); is_binary(Name) ->
	{ok, Request} = ewok_dns:encode(#dns_query{
		id = ewok_util:unow(),
		questions = [#dns_rr{name = Name}]
	}),
	{ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
	ok = gen_udp:send(Socket, ?LOCAL, ?DNS_PORT, Request),
	{ok, {Address, Port, Data}} = gen_udp:recv(Socket, 0, 5000),
	?TTY({Address, Port, Data}),
	{ok, Response} = ewok_dns:decode(Data),
	gen_udp:close(Socket),
	Response.
	

%%% test client
send() ->
	send(<<"test">>).
send(Bin) when is_binary(Bin) ->
	send(Bin, ?DNS_PORT);
send(Term) ->
	send(term_to_binary(Term)).

send(Request, Port) ->
	{ok, Socket} = gen_udp:open(0, [binary, {active, false}]),
	ok = gen_udp:send(Socket, "localhost", Port, Request),
	Value = gen_udp:recv(Socket, 0, 5000),
	gen_udp:close(Socket),
	Value.


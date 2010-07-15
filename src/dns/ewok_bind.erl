-module(ewok_bind).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("dns.hrl").

%-include_lib("kernel/src/inet_dns.hrl").

-behaviour(ewok_inet).
-export([start/1, init/2, terminate/3]).

-record(state, {remote_ip, remote_port}).

start(Port) -> 
	#ewok_inet{
		id = ?MODULE,
		transport = udp,
		port = Port,
		protocol = dns,
		handler = ?MODULE,
		codec = ewok_dns,
		timeout = 10
	}.
	
init(#dns_query{id=ID, questions=Names}, {RemoteIP, RemotePort}) ->
	Response = lookup(Names),
%	?TTY({reply, ID, Response}),
	{reply, Response#dns_response{id=ID}, terminate, #state{remote_ip=RemoteIP, remote_port=RemotePort}}.

terminate(_, _, _) ->
	ok.

lookup(_Names) ->
	#dns_response{id = ewok_util:unow(), code=not_implemented}.

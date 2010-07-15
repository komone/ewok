%%
-module(ewok_uspd).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("usp.hrl").

%% Universal Service Protocol
%% Suggested IANA Port 30
%% Suggested Well-known ports 3300-3301
%% Stateless?

-define(USP_PORT, 30).

-export([start/0, start/1]).

-behaviour(ewok_inet).
-export([init/2, terminate/3]).
-export([connected/2]).

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

-record(state, {remote_ip, remote_port}).

start() ->
	start(?USP_PORT).
start(Port) ->
	#ewok_inet{
		transport = tcp,
		protocol = usp,
		port = Port,
		handler = ewok_usp,
		codec = ewok_usp,
		timeout = 10
	}.

%% Callbacks: ewok_inet
init(_Options, {RemoteIP, RemotePort}) ->
	{noreply, connected, #state{remote_ip = RemoteIP, remote_port = RemotePort}}.	
%%
connected(Request, State) ->
	?TTY({usp, Request}),
	{reply, ok, connected, State}.
%%
terminate(_Reason, _NextState, _StateData) ->
	ok.

%% ewok_codec
encode(Term) ->
	{ok, term_to_binary(Term)}.
	
decode(Bin) ->
	{ok, binary_to_term(Bin, [safe])}.

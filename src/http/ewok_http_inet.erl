%%
-module(ewok_http_inet).

-compile(export_all).

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_inet).
-export([init/1, terminate/3]).

test() ->
	ewok_logging_srv:start_link([]),
	ewok_db:start(),
	Default = #ewok_inet{
		name = ?MODULE,
		port = 8080,
		protocol = http,
		handler = ?MODULE,
		codec = ewok_http_codec
	},
	ewok_inet:start_link(Default).

%% callbacks: ewok_inet
init(_Options) ->
	{ok, connected, _State = []}.

connected(Request, State) ->
	?TTY({request, Request}),
	ewok_log:message(?MODULE, {request, Request}),
	{noreply, connected, State}.

websocket(_Request, State) ->
	{reply, ok, websocket, State}.
	
terminate(_Reason, _NextState, _StateData) ->
	ok.
	
%% ewok_codec callbacks

	

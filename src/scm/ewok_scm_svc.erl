-module(ewok_scm_svc).

-behaviour(ewok_net).
-export([init/1, encode/1, decode/1, request/2]).

-export([start/1, stop/0]).

start(Opts) ->
	ewok_net:start_link(?MODULE, Opts).
stop() ->
	ewok_net:stop(?MODULE).

decode(_Bin) ->
	ok.
encode(_Term) ->
	ok.

request(_X, _Y) ->
	ok.

init(_Opts) ->
	ok.

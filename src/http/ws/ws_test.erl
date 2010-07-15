-module(ws_test).
-include("ewok.hrl").

-behaviour(ewok_http_resource).
-export([filter/1, resource_info/0, add/2]).

resource_info() ->
	[].
	
filter(_Request) ->
	{delegate, ewok_webservice, [?MODULE]}.

add(X, Y) ->
	X + Y.

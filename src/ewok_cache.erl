%% PRE_CACHE everything!
-module(ewok_cache).
-vsn("1.0").
-author('steve@simulacity.com').

-include("ewok.hrl").

%% Cache API
-export([add/1, clear/1, reset/0, lookup/1, lookup/2]).

-define(SERVER, ewok_cache_srv).

%
add(Records) when is_list(Records) ->
	X = [add(X) || X <- Records],
	{ok, length(X)};
add(Record) 
		when is_tuple(Record) 
		andalso is_atom(element(1, Record)) 
		andalso size(Record) > 1 ->
	gen_server:call(?SERVER, {add, Record}, infinity).

clear(Type) when is_atom(Type) ->
	gen_server:call(?SERVER, {clear, Type}, infinity).
	
reset() ->
	gen_server:call(?SERVER, {reset}, infinity).

lookup(Type) ->
	case lists:member(Type, ets:all()) of
	true -> lists:sort(ets:tab2list(Type));
	false -> undefined
	end.
lookup(Type, Key) ->
	case lists:member(Type, ets:all()) of
	true ->
		case ets:lookup(Type, Key) of
		[R] -> R;
		[] -> undefined 
		end;
	false -> undefined
	end.

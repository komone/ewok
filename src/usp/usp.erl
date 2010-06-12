-module(usp).
-include("usp.hrl").

-export([validate/1, assert/2]).

validate([_H = #usp_type{}|T]) ->
	validate(T);
validate([H = #usp_operation{}|T]) ->
	case check(H#usp_operation.out) of
	ok ->
		validate(T);
	error ->
		{error, invalid}
	end;
validate([]) ->
	ok.

check(atom) -> ok;
check(integer) -> ok;
check(float) -> ok;
check(binary) -> ok;
check(list) -> ok;
check(_) -> error.

assert(_X, _Regex) ->
	false.

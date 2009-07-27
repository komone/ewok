-module(esp_validator).

-include("ewok.hrl").

-compile(export_all).

%%	
validate(Params, Validator) when is_atom(Validator) ->
	validate(Params, [Validator]);
validate(Params, [Validator|T]) ->
	Pred = get_validator(Validator),
	case lists:dropwhile(Pred, Params) of
	[] -> validate(Params, T);
	_ -> false
	end;
validate([], _) ->
	false;
validate(_Params, []) ->
	true.

get_validator(not_null) -> fun not_null/1;
get_validator(_) -> error.

%
not_null(Param) -> 
	?is_string(Param).



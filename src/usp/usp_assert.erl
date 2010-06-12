-module(usp_assert).

-compile(export_all).


length(Value, Max) ->
	length(Value, 0, Max).
	
length(Value, Min, Max) when is_binary(Value), size(Value) >= Min, size(Value) =< Max ->
	true;
length(Value, Min, Max) when is_tuple(Value), size(Value) >= Min, size(Value) =< Max ->
	true;
length(Value, Min, Max) when is_list(Value), length(Value) >= Min, length(Value) =< Max ->
	true;
length(_, _, _) ->
	false.


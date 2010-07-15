%% Copyright 2010 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(ewok_text).
-include("ewok.hrl").

-export([encode/1, trim/1, split/2, split/3, replace/3, strip/2, remove/2, unquote/1, 
	is_upper/1, is_lower/1, to_upper/1, to_lower/1, eval/1, match/2, interleave/2]).

%%
encode(X) when is_binary(X)  -> 
	X;
encode(X) -> 
	list_to_binary(io_lib:format("~p", [X])).

%% Currently, just simple integers and floats
eval(X) ->
	X1 = trim(X),
	case re:run(X1, <<"^-?[0-9]*\\.[0-9]+?$">>) of
	{match, _} ->
		list_to_float(binary_to_list(<<X1/binary>>));
	nomatch ->
		case re:run(X1, <<"^-?[0-9]+$">>) of
		{match, _} ->
			list_to_integer(binary_to_list(X1));
		nomatch ->
			undefined
		end
	end.

%%
match(Bin, Regex) ->
	case re:run(Bin, Regex) of
	{match, _} ->
		true;
	nomatch ->
		false
	end.

%%
split(Bin, Regex) ->
	split(Bin, Regex, []).
split(Bin, Regex, Parts) when is_integer(Parts) ->
	split(Bin, Regex, [{parts, Parts}]);
split(Bin, Regex, all) ->
	re:split(Bin, Regex, []);
split(Bin, Regex, Opts) ->
	[X || X <- re:split(Bin, Regex, Opts), X =/= <<>>].	

%%
replace(Bin, Regex, Value) ->
	re:replace(Bin, Regex, Value, [{return, binary}, global]).
%%
strip(Bin, Regex) ->
	replace(Bin, Regex, <<>>).
%% @deprecated: use strip/2
remove(Bin, Regex) ->
	replace(Bin, Regex, <<>>).
%%
unquote(Bin) ->
	replace(Bin, <<"^\"|\"$">>, <<"">>).
%%
interleave([H|T], Separator) ->
	interleave(T, Separator, [H]).
interleave([H|T], Separator, Acc) ->
	interleave(T, Separator, [H, Separator | Acc]);
interleave([], _, Acc) ->
	list_to_binary(lists:reverse(Acc)).


%% hmmm
trim(S) when ?is_string(S) ->
	trim(list_to_binary(S));
trim(S) when is_binary(S) -> 
	% @thanks Seth Falcon
	replace(S, <<"^\\s+|\\s+$">>, <<"">>);
trim(S) -> 
	S.

%%
is_upper(<<C>>) -> is_upper(C);
is_upper([C]) -> is_upper(C);
is_upper(C) when C >= $A, C =< $Z -> true;
is_upper(C) when C >= 16#C0, C =< 16#D6 -> true;
is_upper(C) when C >= 16#D8, C =< 16#DE -> true;
is_upper(_) -> false.

%% 
to_upper(C) when is_integer(C) -> to_upper(<<C>>);
to_upper(S) when ?is_string(S) -> string:to_upper(S);
to_upper(B) when is_binary(B) -> bin_to_upper(B, <<>>);
to_upper(S) -> S.
% @private
bin_to_upper(<<C, Rest/binary>>, Acc) ->
	U = uppercase(C),
	bin_to_upper(Rest, <<Acc/binary, U>>);
bin_to_upper(<<>>, Acc) ->
	Acc.
%% IMPL: Latin1
uppercase(C) when C >= $a, C =< $z -> C - 32;
uppercase(C) when C >= 16#E0, C =< 16#F6 -> C - 32;
uppercase(C) when C >= 16#F8, C =< 16#FE -> C - 32;
uppercase(C) -> C.

%% 
is_lower(<<C>>) -> is_lower(C);
is_lower([C]) -> is_lower(C);
is_lower(C) when C >= $a, C =< $z -> true;
is_lower(C) when C >= 16#E0, C =< 16#F6 -> true;
is_lower(C) when C >= 16#F8, C =< 16#FE -> true;
is_lower(_) -> false.
	
%%
to_lower(C) when is_integer(C) -> lowercase(C);
to_lower(S) when ?is_string(S) -> string:to_lower(S);
to_lower(B) when is_binary(B) -> bin_to_lower(B, <<>>);
to_lower(V) -> V.
% @private
bin_to_lower(<<C, Rest/binary>>, Acc) ->
	C1 = lowercase(C),
	bin_to_lower(Rest, <<Acc/binary, C1>>);
bin_to_lower(<<>>, Acc) ->
	Acc.
%% IMPL: Latin1	
lowercase(C) when C >= $A, C =< $Z -> C + 32;
lowercase(C) when C >= 16#C0, C =< 16#D6 -> C + 32;
lowercase(C) when C >= 16#D8, C =< 16#DE -> C + 32;
lowercase(C) -> C.

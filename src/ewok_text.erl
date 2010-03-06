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

-export([trim/1, split/2, split/3, replace/3, unquote/1, 
	is_upper/1, is_lower/1, to_upper/1, to_lower/1,
	value/1, hex/1, unhex/1, hex2int/1]).

%%
split(Bin, Regex) ->
	split(Bin, Regex, []).
split(Bin, Regex, Parts) when is_integer(Parts) ->
	split(Bin, Regex, [{parts, Parts}]);
split(Bin, Regex, Opts) ->
	[X || X <- re:split(Bin, Regex, Opts), X =/= <<>>].	

%%
replace(Bin, Regex, Value) ->
	re:replace(Bin, Regex, Value, [{return, binary}, global]).

%%
unquote(Bin) ->
	replace(Bin, <<"^\"|\"$">>, <<"">>).

%% hmmm
trim(S) when ?is_string(S) ->
	string:strip(S);
trim(S) when is_binary(S) -> 
	% @thanks Seth Falcon
	replace(S, <<"^\\s+|\\s+$">>, <<"">>);
trim(S) -> 
	S.

%%
value(X) when is_binary(X)  -> X;
value(X) when is_atom(X)    -> atom_to_binary(X, utf8);
value(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
value(X) when is_float(X)   -> list_to_binary(float_to_list(X));
%value(X) when ?is_string(X) -> list_to_binary(X);
value(X) when is_tuple(X)   -> list_to_binary(io_lib:format("~p", [X]));
value(X) -> list_to_binary(io_lib:format("~p", [X])).

%% in ewok_xml the above is...
%makeio(X) when is_integer(X) -> makeio(integer_to_list(X));
%makeio(X) when is_float(X)   -> makeio(float_to_list(X));
%makeio(X) when is_atom(X)    -> atom_to_binary(X, utf8);
%makeio(X) when is_list(X)    -> list_to_binary(X);
%makeio(X) when is_binary(X)  -> X.

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

%%
hex(Bin) when is_binary(Bin) ->
	hex(Bin, <<>>).
hex(<<A:4, B:4, Rest/binary>>, Acc) ->
	U = hexdigit(A),
	L = hexdigit(B),
	hex(Rest, <<Acc/binary, U, L>>);
hex(<<>>, Acc) ->
	Acc.

% @private
hexdigit(D) when D >= 0, D =< 9 -> $0 + D;
hexdigit(D) when D >= 10, D =< 16 -> $a + D - 10.

%%
hex2int(Bin) when is_binary(Bin) ->
	hex2int(Bin, 0).
hex2int(<<A, Rest/binary>>, Acc) ->
	hex2int(Rest, unhexdigit(A) + Acc * 16);
hex2int(<<>>, Acc) ->
	Acc.

unhex(Bin) when is_binary(Bin) ->
	unhex(Bin, <<>>).
unhex(<<A, B, Rest/binary>>, Acc) ->
	I = unhexdigit(A) * 16 + unhexdigit(B),
	unhex(Rest, <<Acc/binary, I>>);
unhex(<<>>, Acc) ->
	Acc.

% @private
unhexdigit(H) when H >= $0, H =< $9 -> H - $0;
unhexdigit(H) when H >= $a, H =< $f -> H - $a + 10;
unhexdigit(H) when H >= $A, H =< $F -> H - $A + 10.

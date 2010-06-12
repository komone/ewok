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

-module(ewok_hex).
-include("ewok.hrl").

-export([encode/1, decode/1, get_value/1]).

%%
encode(Bin) when is_binary(Bin) ->
	encode(Bin, <<>>).
encode(<<A:4, B:4, Rest/binary>>, Acc) ->
	U = encode_digit(A),
	L = encode_digit(B),
	encode(Rest, <<Acc/binary, U, L>>);
encode(<<>>, Acc) ->
	Acc.

% @private
encode_digit(D) when D >= 0, D =< 9 -> 
	$0 + D;
encode_digit(D) when D >= 10, D =< 16 -> 
	$a + D - 10.

%%
decode(Bin) when is_binary(Bin) ->
	decode(Bin, <<>>).
decode(<<A, B, Rest/binary>>, Acc) ->
	I = decode_digit(A) bsl 4 bor decode_digit(B),
	decode(Rest, <<Acc/binary, I>>);
decode(<<>>, Acc) ->
	Acc.

% @private
decode_digit(H) when H >= $0, H =< $9 -> 
	H - $0;
decode_digit(H) when H >= $a, H =< $f -> 
	H - $a + 10;
decode_digit(H) when H >= $A, H =< $F -> 
	H - $A + 10.

%%
get_value(Bin) when is_binary(Bin) ->
	erlang:list_to_integer(binary_to_list(Bin), 16).

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

-module(ewok_crypto).
-export([encrypt/3, decrypt/3]).
-export([test/0]).

-define(IVEC, <<213,53,164,93,158,212,70,56,134,80,224,220,249,214,82,76>>).

% #define DELTA 0x9e3779b9
-define(DELTA, 16#9E3779B9).

%% 
test() ->
	Key = ewok_identity:random(),
	Cipher = encrypt(aes, Key, <<"Arbow">>),
    Plaintext = decrypt(aes, Key, Cipher),
	{Cipher, Plaintext}.

%%
encrypt(xxtea, Key, Plaintext) ->
	cbtea_encode(Plaintext, binary_to_list(Key));
encrypt(aes, <<Key:16/binary>>, Bin) ->
	list_to_binary(aes_encode(Bin, Key, [])). 

%% 
decrypt(xxtea, Ciphertext, Key) ->	
	cbtea_decode(Ciphertext, binary_to_list(Key));
decrypt(aes, Key, Bin) ->
	try 
		Value = aes_decode(Bin, Key, []),
		[Text] = ewok_util:split(list_to_binary(Value), <<"\\x">>),
		Text
	catch
	_:_ -> 
		{error, invalid_key}
	end.

aes_encode(<<>>, _Key, Acc) ->
	lists:reverse(Acc);
aes_encode(<<Block:16/binary, Rest/binary>>, Key, Acc) ->
	Cipher = crypto:aes_cfb_128_encrypt(?IVEC, Key, Block),
	aes_encode(Rest, Key, [Cipher|Acc]);
aes_encode(Rest, Key, Acc) ->
	Pad = list_to_binary(lists:duplicate(16 - size(Rest), <<0>>)),
	Block = <<Rest/binary, Pad/binary>>,
	Cipher = crypto:aes_cfb_128_encrypt(?IVEC, Key, Block),
	lists:reverse([Cipher|Acc]).

aes_decode(<<>>, _Key, Acc) ->
	lists:reverse(Acc);
aes_decode(<<Block:16/binary, Rest/binary>>, Key, Acc) ->
	Cipher = crypto:aes_cfb_128_decrypt(?IVEC, Key, Block),
	aes_decode(Rest, Key, [Cipher|Acc]).


% #define MX ((z >> 5 ^ y << 2) + (y >> 3 ^ z << 4)) ^ ((sum ^ y) + (k[(p & 3) ^ e] ^ z));
mix(K, Z, Y, Sum, P) ->
	X1 = (Z bsr 5) band 16#07FFFFFF bxor (Y bsl 2),
	X2 = (Y bsr 3) band 16#1FFFFFFF bxor (Z bsl 4),
	X3 = lists:nth((P band 3 bxor (Sum bsr 2) band 3) + 1, K),
	int32(X1 + X2) bxor int32((Sum bxor Y) + (X3 bxor Z)).
		
%%
cbtea_encode(Plaintext, Key) ->
	Value = binary_to_list(Plaintext),
	Rounds = 6 + trunc(52 / length(Value)),
	cbtea_encode1(Value, Key, Rounds, ?DELTA).	
%
cbtea_encode1(Value, Key, Rounds, Sum) when Rounds > 0 ->
	Value1 = cbtea_encode2(Value, Key, 0, Sum),
	MX = mix(Key, lists:last(Value), hd(Value1), Sum, length(Value) - 1),
	Z = int32(lists:last(Value1) + MX),
	Value2 = set_last(Value1, Z),
	cbtea_encode1(Value2, Key, Rounds - 1, int32(Sum + ?DELTA));
%
cbtea_encode1(Value, _Key, 0, _Sum) ->
	int32_list_to_binary(Value).
%%
cbtea_encode2(Value, Key, Count, Sum) when Count + 1 < length(Value) ->
	H = lists:sublist(Value, Count),
	[Z, Y | T] = lists:nthtail(Count, Value),
	MX = mix(Key, Z, Y, Sum, Count),
	Value1 = H ++ [int32(Z + MX), Y|T],
	cbtea_encode2(Value1, Key, Count + 1, Sum);
cbtea_encode2(Value, _, _, _)  ->
	Value.

%
set_last(List, Z) ->
	[_H|T]  = lists:reverse(List),
	lists:reverse([Z|T]).
set_first(List, Z) ->
	[_H|T] = List,
	[Z|T].

binary_to_int32_list(<<X:32, Rest/binary>>, Acc) ->
	binary_to_int32_list(Rest, [X|Acc]);
binary_to_int32_list(<<>>, Acc) ->
	lists:reverse(Acc).
int32_list_to_binary(List) ->	
	list_to_binary([<<X:32>> || X <- List]).

int32(Num) -> 
    Int = Num band 16#FFFFFFFF, 
    case Int > 16#7FFFFFFF of
		true -> Int - 16#FFFFFFFF - 1;
        false -> Int
    end.
	
cbtea_decode(Ciphertext, Key) ->
	Value = binary_to_int32_list(Ciphertext, []),
	Rounds = 6 + trunc(52 / length(Value)),
	cbtea_decode1(Value, Key, Rounds, Rounds * ?DELTA).
	
cbtea_decode1(Value, Key, Rounds, Sum) when Rounds > 0 ->
	Value1 = cbtea_decode2(Value, Key, length(Value), Sum),
	MX = mix(Key, lists:last(Value), hd(Value1), Sum, length(Value) - 1),
	Y = int32(hd(Value1) - MX),
	Value2 = set_first(Value1, Y),
	cbtea_decode1(Value2, Key, Rounds - 1, int32(Sum - ?DELTA));
cbtea_decode1(Value, _Key, 0, 0) ->
	int32_list_to_binary(Value).

cbtea_decode2(Value, Key, Count, Sum) when Count > 1 ->
	H = lists:sublist(Value, Count - 2),
	[Z, Y | T] = lists:nthtail(Count - 2, Value),
	MX = mix(Key, Z, Y, Sum, Count),
	Value1 = H ++ [Z, int32(Y - MX)|T],
	cbtea_decode2(Value1, Key, Count - 1, Sum);
cbtea_decode2(Value, _, _, _)  ->
	Value.


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
-include("ewok.hrl").

-export([start/0, sign/3, encrypt/3, decrypt/3, hotp/1, hotp/2]).

-export([hotp_test/0, xxtea_test/0, xxtea/0]).

%% AES
-define(IVEC, <<213,53,164,93,158,212,70,56,134,80,224,220,249,214,82,76>>).
% XXTEA
-define(DELTA, 16#9E3779B9).
% HMAC SHA-256
-define(SHA_BLOCKSIZE, 64).

%%
start() ->
	crypto:start().
	
%%
encrypt(xxtea, Key, Plaintext) ->
	cbtea_encode(Plaintext, binary_to_list(Key));
encrypt(aes, <<Key:16/binary>>, Bin) ->
	list_to_binary(aes_encode(Bin, Key, [])). 

xxtea_test() ->
	%% http://www.crypt.co.za/post/27
	Vectors = [
		{<<"00000000000000000000000000000000">>, <<"0000000000000000">>, <<"ab043705808c5d57">>},
		{<<"0102040810204080fffefcf8f0e0c080">>, <<"0000000000000000">>, <<"d1e78be2c746728a">>},
		{<<"9e3779b99b9773e9b979379e6b695156">>, <<"ffffffffffffffff">>, <<"67ed0ea8e8973fc5">>},
		{<<"0102040810204080fffefcf8f0e0c080">>, <<"fffefcf8f0e0c080">>, <<"8c3707c01c7fccc4">>}
	],
	[xxtea_test(ewok_hex:decode(Key), ewok_hex:decode(Text), ewok_hex:decode(Cipher)) || {Key, Text, Cipher} <- Vectors],
	ok.

xxtea_test(Key, Text, Cipher) ->
	Cipher = encrypt(xxtea, Key, Text),
	Text = decrypt(xxtea, Key, Cipher).

%% 
decrypt(xxtea, Key, Ciphertext) ->	
	cbtea_decode(Ciphertext, binary_to_list(Key));
decrypt(aes, Key, Bin) ->
	try 
		Value = aes_decode(Bin, Key, []),
		[Text] = ewok_text:split(list_to_binary(Value), <<"\\x">>),
		Text
	catch
	_:_ -> 
		{error, invalid_key}
	end.
%%
sign(hmac256, Key, Data) ->
	hmac256(Key, Data).
	

%%
hotp(Key) ->
	T = ewok_util:unow() div 60,
	hotp(Key, T).

%% RFC-4226 "HOTP: An HMAC-Based One-Time Password Algorithm"
%% http://tools.ietf.org/html/rfc4226
hotp(Key, Count) ->
	HS = crypto:sha_mac(Key, <<Count:64>>),
	<<_:19/binary, _:4, Offset:4>> = HS,
	<<_:Offset/binary, _:1, P:31, _/binary>> = HS,
	HOTP = integer_to_list(P rem 1000000),
	Pad = lists:duplicate(6 - length(HOTP), $0),
	list_to_binary([Pad, HOTP]).

%% from Appendix D - HOTP Algorithm: Test Values
hotp_test() ->
	Key = <<"12345678901234567890">>,
	<<"755224">> = hotp(Key, 0),
	<<"287082">> = hotp(Key, 1),
	<<"359152">> = hotp(Key, 2),
	<<"969429">> = hotp(Key, 3),
	<<"338314">> = hotp(Key, 4),
	<<"254676">> = hotp(Key, 5),
	<<"287922">> = hotp(Key, 6),
	<<"162583">> = hotp(Key, 7),
	<<"399871">> = hotp(Key, 8),
	<<"520489">> = hotp(Key, 9),
	ok.

%% AES CFB
%% @private
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
%% @private
aes_decode(<<>>, _Key, Acc) ->
	lists:reverse(Acc);
aes_decode(<<Block:16/binary, Rest/binary>>, Key, Acc) ->
	Cipher = crypto:aes_cfb_128_decrypt(?IVEC, Key, Block),
	aes_decode(Rest, Key, [Cipher|Acc]).


%% @private
% HMAC test vectors at http://www.rfc-archive.org/getrfc.php?rfc=4231
hmac256(Key, Data) when is_binary(Key), is_binary(Data) ->
	HashKey = 
		case ?SHA_BLOCKSIZE - size(Key) of
		X when X < 0 ->
			KeyDigest = sha2:hexdigest256(Key),
			Pad = ?SHA_BLOCKSIZE - size(KeyDigest),
			<<KeyDigest/binary, 0:(Pad * 8)>>;
		X when X > 0 ->
			<<Key/binary, 0:(X * 8)>>;
		X when X =:= 0 ->
			Key
		end,
	IPad = list_to_binary(lists:duplicate(?SHA_BLOCKSIZE, 16#36)),
	OPad = list_to_binary(lists:duplicate(?SHA_BLOCKSIZE, 16#5c)),
	%% H(K XOR opad, H(K XOR ipad, text))
	H1 = sha2:hexdigest256(<<(crypto:exor(HashKey, IPad))/binary, Data/binary>>),
	sha2:hexdigest256(<<(crypto:exor(HashKey, OPad))/binary, H1/binary>>).

xxtea() -> "
#define DELTA 0x9e3779b9
#define MX ((z>>5^y<<2) + (y>>3^z<<4)) ^ ((sum^y) + (k[(p&3)^e] ^ z));
void btea(uint32_t *v, int n, uint32_t const k[4]) {
	uint32_t y, z, sum;
	unsigned p, rounds, e;
	if (n > 1) {          /* Coding Part */
		rounds = 6 + 52/n;
		sum = 0;
		z = v[n-1];
		do {
			sum += DELTA;
			e = (sum >> 2) & 3;
			for (p=0; p<n-1; p++)
			y = v[p+1], z = v[p] += MX;
			y = v[0];
			z = v[n-1] += MX;
		} while (--rounds);
	} else if (n < -1) {  /* Decoding Part */
		n = -n;
		rounds = 6 + 52/n;
		sum = rounds*DELTA;
		y = v[0];
		do {
			e = (sum >> 2) & 3;
			for (p=n-1; p>0; p--)
			z = v[p-1], y = v[p] -= MX;
			z = v[n-1];
			y = v[0] -= MX;
		} while ((sum -= DELTA) != 0);
	}
}
".

%% XXTEA
% #define MX ((z >> 5 ^ y << 2) + (y >> 3 ^ z << 4)) ^ ((sum ^ y) + (k[(p & 3) ^ e] ^ z));
%% @private
mx(K, Z, Y, Sum, P) ->
	X1 = (Z bsr 5) band 16#07FFFFFF bxor (Y bsl 2),
	X2 = (Y bsr 3) band 16#1FFFFFFF bxor (Z bsl 4),
	X3 = lists:nth((P band 3 bxor (Sum bsr 2) band 3) + 1, K),
	int32(X1 + X2) bxor int32((Sum bxor Y) + (X3 bxor Z)).
		
%% @private
cbtea_encode(Plaintext, Key) ->
	Value = binary_to_list(Plaintext),
	Rounds = 6 + trunc(52 / length(Value)),
	cbtea_encode1(Value, Key, Rounds, ?DELTA).	
%
cbtea_encode1(Value, Key, Rounds, Sum) when Rounds > 0 ->
	Value1 = cbtea_encode2(Value, Key, 0, Sum),
	MX = mx(Key, lists:last(Value), hd(Value1), Sum, length(Value) - 1),
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
	MX = mx(Key, Z, Y, Sum, Count),
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
	
%% @private
cbtea_decode(Ciphertext, Key) ->
	Value = binary_to_int32_list(Ciphertext, []),
	Rounds = 6 + trunc(52 / length(Value)),
	cbtea_decode1(Value, Key, Rounds, Rounds * ?DELTA).
	
cbtea_decode1(Value, Key, Rounds, Sum) when Rounds > 0 ->
	Value1 = cbtea_decode2(Value, Key, length(Value), Sum),
	MX = mx(Key, lists:last(Value), hd(Value1), Sum, length(Value) - 1),
	Y = int32(hd(Value1) - MX),
	Value2 = set_first(Value1, Y),
	cbtea_decode1(Value2, Key, Rounds - 1, int32(Sum - ?DELTA));
cbtea_decode1(Value, _Key, 0, 0) ->
	int32_list_to_binary(Value).

cbtea_decode2(Value, Key, Count, Sum) when Count > 1 ->
	H = lists:sublist(Value, Count - 2),
	[Z, Y | T] = lists:nthtail(Count - 2, Value),
	MX = mx(Key, Z, Y, Sum, Count),
	Value1 = H ++ [Z, int32(Y - MX)|T],
	cbtea_decode2(Value1, Key, Count - 1, Sum);
cbtea_decode2(Value, _, _, _)  ->
	Value.


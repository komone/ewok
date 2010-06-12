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

-module(xxtea).
-include("ewok.hrl").
-compile(export_all).

% #define DELTA 0x9e3779b9
-define(DELTA, 16#9e3779b9).
test() ->
	Vectors = [
		{<<"00000000000000000000000000000000">>, <<"0000000000000000">>, <<"ab043705808c5d57">>},
		{<<"0102040810204080fffefcf8f0e0c080">>, <<"0000000000000000">>, <<"d1e78be2c746728a">>},
		{<<"9e3779b99b9773e9b979379e6b695156">>, <<"ffffffffffffffff">>, <<"67ed0ea8e8973fc5">>},
		{<<"0102040810204080fffefcf8f0e0c080">>, <<"fffefcf8f0e0c080">>, <<"8c3707c01c7fccc4">>}
	],
	[test(ewok_hex:decode(Key), ewok_hex:decode(Text), ewok_hex:decode(Cipher)) || {Key, Text, Cipher} <- Vectors],
	ok.

%
test(Key, Value, Cipher) ->
	Cipher1 = encode(Key, Value),
	?TTY({Key, Value, Cipher1, Cipher}).
%	Text = decrypt(xxtea, Key, Cipher).

%% ab043705 808c5d57
encode(Key, Value) ->
	N = size(Value) div 4,
	Rounds = 6 + (52 div N),
	?TTY({rounds, Rounds}),
	do_encode(Key, Value, N, 0, Rounds).
%
do_encode(K, V, N, Sum, Rounds) when Rounds > 0 ->
	S = Sum + ?DELTA,
	E = (S bsr 2) band 3,
	V1 = for_encode(K, V, S, E, 0, N - 1),
%	?TTY({sum, S, e, E, v1, V1}),
	do_encode(K, V1, N, S, Rounds - 1);
do_encode(_, V, _, _, 0) ->
	V.
%
for_encode(K, V, Sum, E, P, N) ->
	case P of
	0 ->
		<<Z:32/little, Y:32/little, Rest/binary>> = V,
		Z1 = Z + mx(K, Sum, Y, Z, E, 0),
		V1 = <<Z1:32/little, Y:32/little, Rest/binary>>,
		for_encode(K, V1, Sum, E, P + 1, N);
	_ when P < N ->
		<<X:P/binary-unit:32, Z:32/little, Y:32/little, Rest/binary>> = V,
		Z1 = Z + mx(K, Sum, Y, Z, E, P),
		V1 = <<X/binary-unit:32, Z1:32/little, Y:32/little, Rest/binary>>,
		for_encode(K, V1, Sum, E, P + 1, N);
	N when N > 2 ->
		Skip = N - 2,
		<<Y:32/little, X:Skip/binary-unit:32, Z:32/little>> = V,
		Z1 = Z + mx(K, Sum, Y, Z, E, N),
		<<Y:32/little, X:Skip/binary-unit:32, Z1:32/little>>;
	N ->
		<<Y:32/little, Z:32/little>> = V,
		Z1 = Z + mx(K, Sum, Y, Z, E, N),
		<<Y:32/little, Z1:32/little>>
	end.
	
% #define MX ((z>>5^y<<2) + (y>>3^z<<4)) ^ ((sum^y) + (k[(p&3)^e] ^ z));
mx(K, S, Y, Z, E, P) ->
	X0 = (Z bsr 5) bxor (Y bsl 2),
	X1 = (Y bsr 3) bxor (Z bsl 4),
	Offset = (P band 3) bxor E,
	<<_:Offset/binary-unit:32, X2:32/little, _/binary>> = K,
	(X0 + X1) bxor ((S bxor Y) + (X2 bxor Z)).
	
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
			for (p=0; p<n-1; p++) {
				y = v[p+1];
				z = v[p] += MX;
			}
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
}".

decode(Key, Value) ->
	Rounds = 6 + 52 div size(Value),
	Sum = Rounds * ?DELTA,
	<<Y:32, _/binary>> = Value,
	{Rounds, Y}.

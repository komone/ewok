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

-module(asn_ber).

-export([encode/1, decode/1]).
-compile(export_all).

%% @def ITU-T Rec.X.690 (07/2002) | ISO/IEC 8825-1:2002

%% BER
% | 8 | 7 | 6 | 5-1  |
% | Class |P/C|Number|

% Bits 8|7 -> Class
%  Universal 0|0
%  Application	0|1
%  Context-specific 1|0
%  Private	1|1
% Universal Class Tags
% P = Primitive, C = Constructed (P + 32).
-define(EOC, 0). % End of content
-define(BOOLEAN, 1). 
-define(INTEGER, 2). 
-define(BIT_STRING, 3). 
-define(BIT_STRING_STRUCT, 35).
-define(OCTET_STRING, 4).
-define(OCTET_STRING_STRUCT, 36).
-define(NULL, 5).
-define(OID, 6). 
-define(OBJECT_DESCRIPTOR, 7).
-define(EXTERNAL_STRUCT, 40). 
-define(REAL, 9). 
-define(ENUMERATED, 10). 
-define(EMBEDDED_PDV_STRUCT, 43).
-define(UTF8_STRING, 12).
-define(UTF8_STRING_STRUCT, 44).
-define(RELATIVE_OID, 13).
%% 14, 15 unused
-define(SEQUENCE, 48).
-define(SET, 49).
% NumericString	P/C	18
-define(PRINTABLE_STRING, 19).
-define(PRINTABLE_STRING_STRUCT, 51).
% T61String		P/C	20
% VideotexString	P/C	21
% IA5String		P/C	22
-define(UTC_TIME, 23).
-define(UTC_TIME_STRUCT, 55).
% GeneralizedTime	P/C	24
% GraphicString	P/C	25
% VisibleString	P/C	26
% GeneralString	P/C	27
% UniversalString	P/C	28
% CHARACTER STRING P/C	29
% BMPString		P/C	30

-define(UNIVERSAL, 0).
-define(APPLICATION, 64).
-define(CONTEXTUAL, 128).
-define(PRIVATE, 192).
-define(CONSTRUCTED, 32).

%%
encode(null) ->
	<<?NULL, 0>>;
encode(eof) ->
	<<?EOC, 0>>;
encode(true) ->
	<<?BOOLEAN, 1, 255>>;
encode(false) ->
	<<?BOOLEAN, 1, 0>>;
encode(X) when is_integer(X) ->
	Size = count_bytes(X),
	<<?INTEGER, Size, X:Size/big-unit:8>>;
encode({application, Id, Content}) when is_list(Content) ->
	Octet = ?APPLICATION bor ?CONSTRUCTED bor Id,
	Bin = list_to_binary([encode(X) || X <- Content]),
	<<Octet, (size(Bin)), Bin/binary>>;
encode({application, Id, Content}) ->
	Octet = ?APPLICATION bor Id,
	Bin = encode(Content),
	<<Octet, (size(Bin)), Bin/binary>>;
encode(OID) when is_tuple(OID) ->
	Bin = encode_oid(tuple_to_list(OID), []),
	<<?OID, (size(Bin)), Bin/binary>>;
encode(Sequence) when is_list(Sequence) ->
	Bin = list_to_binary([encode(X) || X <- Sequence]),
	<<?SEQUENCE, (size(Bin)), Bin/binary>>; 
encode(Bin) when is_binary(Bin) ->
	<<?OCTET_STRING, (size(Bin)), Bin/binary>>.

%%
encode_oid([H], Acc) when is_integer(H) ->
	Acc1 = [integer_to_list(H)|Acc],
	list_to_binary(lists:reverse(Acc1));
encode_oid([H|T], Acc) when is_integer(H) ->
	encode_oid(T, [<<$.>>, integer_to_list(H)|Acc]).

%%
count_bytes(X) ->
	count_bytes(X, 1).
count_bytes(X, Acc) ->
	case X bsr 8 of
	0 -> 
		Acc;
	X1 -> 
		count_bytes(X1, Acc + 1)
	end.

%%
decode(Bin) ->
	[Result] = decode(Bin, []),
	{asn, [{encoding, ber}], Result}.
%%	
decode(<<?EOC, 0, Rest/binary>>, Acc) ->
	decode(Rest, [eof|Acc]);
decode(<<?NULL, 0, Rest/binary>>, Acc) ->
	decode(Rest, [null|Acc]);
decode(<<?BOOLEAN, 1, 0, Rest/binary>>, Acc) ->
	decode(Rest, [false|Acc]);
decode(<<?BOOLEAN, 1, _, Rest/binary>>, Acc) ->
	decode(Rest, [true|Acc]);
decode(<<?INTEGER, Length, _/binary>> = Bin, Acc) ->
	<<_, _, X:Length/big-unit:8, Rest/binary>> = Bin,
	decode(Rest, [X|Acc]);
decode(<<?SEQUENCE, Length, _/binary>> = Bin, Acc) ->
	<<_, _, X:Length/binary, Rest/binary>> = Bin,
	Sequence = decode(X, []),
	decode(Rest, [{sequence, Sequence}|Acc]);
decode(<<?OCTET_STRING, Length, _/binary>> = Bin, Acc) ->
	<<_, _, X:Length/binary, Rest/binary>> = Bin,
	decode(Rest, [X|Acc]);
decode(<<?PRINTABLE_STRING, Length, _/binary>> = Bin, Acc) ->
	<<_, _, X:Length/binary, Rest/binary>> = Bin,
	decode(Rest, [X|Acc]);
decode(<<?CONTEXTUAL, Length, _/binary>> = Bin, Acc) ->
	<<_, _, X:Length/binary, Rest/binary>> = Bin,
	decode(Rest, [X|Acc]);
decode(<<0:1, 1:1, _Struct:1, Tag:5, Length, _/binary>> = Bin, Acc) ->
	<<_, _, X:Length/binary, Rest/binary>> = Bin,
	Parts = decode(X, []),
	decode(Rest, [{application, Tag, Parts}|Acc]);
decode(<<>>, Acc) ->
	lists:reverse(Acc).


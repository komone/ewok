%% Copyright 2010 Steve Davis <steve@simulacity.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(ewok_dns).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("dns.hrl").

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

-compile(export_all).

-define(QUERY, 0).
-define(RESPONSE, 1).
-define(HEADER_BYTES, 12).

-record(dns_header, {id, qr, opcode = status, aa = 0, tc = 0, rd = 0, ra = 0, z = 0, rcode, qdcount = 0, ancount = 0, nscount = 0, arcount = 0}).

%% @ref http://tools.ietf.org/html/rfc1035
testq() ->
	<<218,142,1,0,0,1,0,0,0,0,0,0,10,115,101,99,111,110,100,108,105,102,101,3,99,111,109,0,0,1,0,1>>.
	
testa() ->
	<<218,142,129,128,0,1,0,2,0,4,0,4,10,115,101,99,111,110,100,108,105,102,101,3,
		99,111,109,0,0,1,0,1,192,12,0,1,0,1,0,0,1,44,0,4,8,4,128,240,192,12,0,1,0,1,
		0,0,1,44,0,4,8,4,128,238,192,12,0,2,0,1,0,0,104,123,0,20,7,110,115,49,45,100,
		102,119,9,108,105,110,100,101,110,108,97,98,192,23,192,12,0,2,0,1,0,0,104,
		123,0,10,7,110,115,48,45,115,102,111,192,84,192,12,0,2,0,1,0,0,104,123,0,10,
		7,110,115,48,45,112,104,120,192,84,192,12,0,2,0,1,0,0,104,123,0,10,7,110,115,
		48,45,100,102,119,192,84,192,152,0,1,0,1,0,0,133,28,0,4,216,82,2,2,192,130,0,
		1,0,1,0,0,133,28,0,4,8,10,145,102,192,108,0,1,0,1,0,1,229,34,0,4,64,154,223,
		32,192,76,0,1,0,1,0,0,133,28,0,4,216,82,2,3>>.
testb() -> 
	<<32,125,129,128,0,1,0,6,0,4,0,0,6,103,111,111,103,108,101,3,99,111,
		109,0,0,1,0,1,192,12,0,1,0,1,0,0,0,176,0,4,74,125,95,147,192,12,0,1,
		0,1,0,0,0,176,0,4,74,125,95,99,192,12,0,1,0,1,0,0,0,176,0,4,74,125,
		95,103,192,12,0,1,0,1,0,0,0,176,0,4,74,125,95,104,192,12,0,1,0,1,0,0,
		0,176,0,4,74,125,95,105,192,12,0,1,0,1,0,0,0,176,0,4,74,125,95,106,
		192,12,0,2,0,1,0,0,224,11,0,6,3,110,115,52,192,12,192,12,0,2,0,1,0,0,
		224,11,0,6,3,110,115,51,192,12,192,12,0,2,0,1,0,0,224,11,0,6,3,110,
		115,49,192,12,192,12,0,2,0,1,0,0,224,11,0,6,3,110,115,50,192,12>>.
testc() ->
	<<61,227,129,128,0,1,0,1,0,4,0,2,3,119,119,119,10,115,105,109,117,
           108,97,99,105,116,121,3,99,111,109,0,0,1,0,1,192,12,0,1,0,1,0,1,81,
           128,0,4,69,164,197,55,192,16,0,2,0,1,0,1,81,128,0,13,3,110,115,51,
           6,108,105,110,111,100,101,192,27,192,16,0,2,0,1,0,1,81,128,0,6,3,
           110,115,49,192,68,192,16,0,2,0,1,0,1,81,128,0,6,3,110,115,52,192,
           68,192,16,0,2,0,1,0,1,81,128,0,6,3,110,115,50,192,68,192,89,0,1,0,
           1,0,1,59,177,0,4,69,93,127,10,192,107,0,1,0,1,0,1,59,254,0,4,207,
           192,70,10>>.
test() ->
	Q = #dns_query{id = 55950, questions = [#dns_rr{name = <<"secondlife.com">>, class = in, type = a}]},
	Ref = testq(),
	{ok, X} = encode(Q),
	?TTY({encode, (X =:= Ref), X, Ref}),
	{ok, A} = decode(Ref),
	?TTY({decode, (A =:= Q), A, Q}).
	
%% ewok_codec callbacks
%%-record(dns_query{id, names = [], type=standard, recurse=true, authenticate=false}).
encode(#dns_query{id=ID, questions=Records, type=Opcode, recurse=Recurse, authenticate=Authenticate}) ->
	Header = encode_header(#dns_header{
		id = ID,
		qr = ?QUERY,
		opcode = opcode(Opcode),
		aa = flag(Authenticate),
		tc = flag(false), %% check truncated
		rd = flag(Recurse),
		ra = 0,
		z = 0, % reserved must be 0
		rcode = response_code(no_error), 
		qdcount = length(Records),
		ancount = 0,
		nscount = 0,
		arcount = 0
	}),
	Questions = encode_questions(Records, []),
	Data = list_to_binary([Header, Questions]),
	{ok, Data};
encode(#dns_response{id=ID, code=Code}) ->
	Header = encode_header(#dns_header{
		id = ID, 
		qr = ?RESPONSE,
		opcode = 0,
		aa = 0, tc = 0, rd = 0, ra = 0, z = 0, 
		rcode = response_code(Code),
		qdcount = 0,
		ancount = 0,
		nscount = 0,
		arcount = 0		
	}), 
	{ok, Header}.
	
encode_header({dns_header, ID, QR, OPCODE, AA, TC, RD, RA, Z, RCODE, QDCOUNT, ANCOUNT, NSCOUNT, ARCOUNT}) ->
	<<ID:16, QR:1, OPCODE:4, AA:1, TC:1, RD:1, RA:1, Z:3, RCODE:4, QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16>>.
	
%% TODO: for now, we are assuming all queries are for IN A records
encode_questions([#dns_rr{name = Name, type = _Type, class = _Class}|T], Acc) ->
	Bin = list_to_binary([encode_name(Name), <<(qtype(a)):16>>, <<(qclass(in)):16>>]),
	encode_questions(T, [Bin | Acc]);
encode_questions([], Acc) ->
	lists:reverse(Acc).
	
encode_name(Name) ->
	Parts = ewok_text:split(Name, <<"\\.">>),
	Data = [<<(size(P)), P/binary>> || P <- Parts],
	StringEnd = <<0:8>>,
	list_to_binary([Data, StringEnd]).
	
%%	
decode(<<Header:?HEADER_BYTES/binary, Records/binary>>) ->
	decode(decode_header(Header), Records).
	
decode_header(<<ID:16, QR:1, OPCODE:4, AA:1, TC:1, RD:1, RA:1, Z:3, RCODE:4, 
		QDCOUNT:16, ANCOUNT:16, NSCOUNT:16, ARCOUNT:16>>) ->
	#dns_header{id = ID, qr = QR, opcode = OPCODE, aa = AA, tc = TC, rd = RD, ra = RA, z = Z, rcode = RCODE, 
		qdcount = QDCOUNT, ancount = ANCOUNT, nscount = NSCOUNT, arcount = ARCOUNT}.

decode(#dns_header{id=ID, qr = ?QUERY, opcode = OPCODE, aa = AA, rd = RD, 
		qdcount = QDCOUNT, ancount = 0, nscount = 0, arcount = 0}, Bin) ->
	Q = #dns_query{
		id = ID, 
		type = deopcode(OPCODE),
		recurse = deflag(RD),
		authenticate = deflag(AA)
	},
	{ok, RR, _} = decode_questions(Bin, QDCOUNT, []),
	{ok, Q#dns_query{questions=RR}};
	
decode(H = #dns_header{id = ID, qr = ?RESPONSE, opcode = OPCODE, rcode = RCODE, 
		qdcount = QDCOUNT, ancount = ANCOUNT, nscount = NSCOUNT, arcount = ARCOUNT}, Bin) ->
	?TTY(H),
	{ok, QD, Bin0} = decode_questions(Bin, QDCOUNT, []),
	{ok, AN, Bin1} = decode_answers(Bin0, ANCOUNT, []),
	{ok, NS, Bin2} = decode_answers(Bin1, NSCOUNT, []),
	{ok, AR, <<>>} = decode_answers(Bin2, ARCOUNT, []),
	{ok, #dns_response{id = ID, type = deopcode(OPCODE), code = deresponse_code(RCODE), 
			questions = QD, answers = AN, authority = NS, additional = AR}}.

decode_answers(<<FLAGS:2, OFFSET:14, TYPE:16, CLASS:16, TTL:32, Size:16, Bin/binary>>, Count, Acc) when FLAGS =:= 3, Count > 0 ->
	Type = detype(TYPE),
	Class = declass(CLASS),
	<<Bin0:Size/binary, Rest/binary>> = Bin,
	Data = decode_rdata(Class, Type, Bin0),
	RR = #dns_rr{name = [{offset, OFFSET - ?HEADER_BYTES}], type = Type, class = Class, ttl = TTL, data = Data},
	decode_answers(Rest, Count - 1, [RR|Acc]);
decode_answers(Bin, 0, Acc) ->
	{ok, lists:reverse(Acc), Bin}.

decode_rdata(in, a, Bin) ->
	<<A, B, C, D>> = Bin,
	{A, B, C, D};
decode_rdata(in, ns, Bin) ->
	{ok, Name, _} = decode_name(Bin, []),
	Name;
decode_rdata(_, _, Bin) ->
	Bin.

decode_name(<<Flags:2, Offset:14, Rest/binary>>, Acc) when Flags =:= 3 ->
	decode_name(Rest, [{offset, Offset - 12}|Acc]);
decode_name(<<Size, Rest/binary>>, Acc) when Size > 0 ->
	<<Part:Size/binary, Rest0/binary>> = Rest,
	decode_name(Rest0, [Part|Acc]);
decode_name(<<0, Rest/binary>>, Acc) ->
	{ok, lists:reverse(Acc), Rest};
decode_name(<<>>, Acc) ->
	{ok, lists:reverse(Acc), <<>>}.
	
decode_questions(Bin, Count, Acc) when Count > 0 ->
	{ok, RR, Rest} = decode_question(Bin, []),
	decode_questions(Rest, Count - 1, [RR|Acc]);
decode_questions(Bin, 0, Acc) ->
	{ok, lists:reverse(Acc), Bin}.

decode_question(RR = <<Length:8, _/binary>>, Acc) when Length > 0 ->
	<<_, NAME:Length/binary, Rest/binary>> = RR,
	decode_question(Rest, [NAME|Acc]);
decode_question(<<0, Rest/binary>>, Acc) ->
%	Name = ewok_text:interleave(lists:reverse(Acc), <<".">>),
	decode_question2(lists:reverse(Acc), Rest).
	
decode_question2(Name, <<TYPE:16, CLASS:16, Rest/binary>>) -> %% TTL:32, RDLENGTH:16, RDATA/binary>>) ->
	{ok, #dns_rr{name = Name, type = deqtype(TYPE), class = declass(CLASS)}, Rest}. %, ttl = TTL, rdlength = RDLENGTH, rdata = RDATA}}.

decode_resource(RR, <<TYPE:16, CLASS:16, TTL:32, RDLENGTH:16, RDATA/binary>>) ->
	<<Data:RDLENGTH/binary, Rest/binary>> = RDATA,
	{ok, RR#dns_rr{type = detype(TYPE), class = declass(CLASS), ttl = TTL, data = Data}, Rest}.

%% util
flag(true)  -> 1;
flag(false) -> 0.

deflag(0) -> false;
deflag(1) -> true.

opcode(standard) -> 0;
opcode(inverse)  -> 1;
opcode(status)   -> 2.

deopcode(0) -> standard;
deopcode(1) -> inverse;
deopcode(2) -> status.

type(a)     -> 1; % a host address
type(ns)    -> 2; % an authoritative name server
type(md)    -> 3; % a mail destination (Obsolete - use MX)
type(mf)    -> 4; % a mail forwarder (Obsolete - use MX)
type(cname) -> 5; % the canonical name for an alias
type(soa)   -> 6; % marks the start of a zone of authority
type(mb)    -> 7; % a mailbox domain name (EXPERIMENTAL)
type(mg)    -> 8; % a mail group member (EXPERIMENTAL)
type(mr)    -> 9; % a mail rename domain name (EXPERIMENTAL)
type(null)  -> 10; % a null RR (EXPERIMENTAL)
type(wks)   -> 11; % a well known service description
type(ptr)   -> 12; % a domain name pointer
type(hinfo) -> 13; % host information
type(minfo) -> 14; % mailbox or mail list information
type(mx)    -> 15; % mail exchange
type(txt)   -> 16. % text strings

detype(1)  -> a;
detype(2)  -> ns;
detype(3)  -> md;
detype(4)  -> mf;
detype(5)  -> cname;
detype(6)  -> soa;
detype(7)  -> mb;
detype(8)  -> mg;
detype(9)  -> mr;
detype(10) -> null;
detype(11) -> wks;
detype(12) -> ptr;
detype(13) -> hinfo;
detype(14) -> minfo;
detype(15) -> mx;
detype(16) -> txt.

qtype(axfr)  -> 252; % A request for a transfer of an entire zone
qtype(mailb) -> 253; % A request for mailbox-related records (MB, MG or MR)
qtype(maila) -> 254; % A request for mail agent RRs (Obsolete - see MX)
qtype('*')   -> 255; % A request for all records
qtype(X)     -> type(X).

deqtype(252) -> axfr;
deqtype(253) -> mailb;
deqtype(254) -> maila;
deqtype(255) -> '*';
deqtype(X) -> detype(X).

class(in) -> 1; % the Internet
class(cs) -> 2; % the CSNET class (Obsolete - used only for examples in some obsolete RFCs)
class(ch) -> 3; % the CHAOS class
class(hs) -> 4. % Hesiod [Dyer 87]

declass(1) -> in;
declass(2) -> cs;
declass(3) -> ch;
declass(4) -> hs.

qclass('*')   -> 255; %  any class
qclass(X) -> class(X). 

deqclass(255) -> '*';
deqclass(X) -> declass(X).

response_code(no_error)        -> 0;
response_code(format_error)    -> 1;
response_code(server_failure)  -> 2;
response_code(name_error)      -> 3;
response_code(not_implemented) -> 4;
response_code(refused)         -> 5.

deresponse_code(0) -> no_error;
deresponse_code(1) -> format_error;
deresponse_code(2) -> server_failure;
deresponse_code(3) -> name_error;
deresponse_code(4) -> not_implemented;
deresponse_code(5) -> refused.

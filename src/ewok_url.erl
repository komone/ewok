-module(ewok_url).

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

%%
decode(Bin) when is_binary(Bin) ->
	list_to_binary(decode(binary_to_list(Bin), []));
decode(List) when is_list(List) ->
	decode(List, []).	
%
decode([$%, Hi, Lo|T], Acc) ->
	try 
		Char = erlang:list_to_integer([Hi, Lo], 16),
		decode(T, [Char|Acc])
	catch
		error:badarg -> {error, invalid_encoding}
	end;
decode([$+|T], Acc) ->
	decode(T, [$ |Acc]);
decode([H|T], Acc) ->
	decode(T, [H|Acc]);
decode([], Acc) ->
	lists:reverse(Acc).
	
%%
encode(Bin) when is_binary(Bin) ->
	list_to_binary(encode(binary_to_list(Bin), []));
encode(List) when is_list(List) ->
	encode(List, []).	
%
encode([H|T], Acc) ->
	encode(T, [encode_char(H)|Acc]);
encode([], Acc) ->
	lists:flatten(lists:reverse(Acc)).

encode_char(C) when C >= $a, C =< $z -> C;
encode_char(C) when C >= $A, C =< $Z -> C;
encode_char(C) when C >= $0, C =< $9 -> C;
encode_char(C = $~) -> C;
encode_char(C = $_) -> C;
encode_char(C = $.) -> C;
encode_char(C = $-) -> C;
encode_char($ ) -> "%20";
encode_char($!) -> "%21";
encode_char($#) -> "%23";
encode_char($$) -> "%24";
encode_char($%) -> "%25";
encode_char($&) -> "%26";
encode_char($') -> "%27";
encode_char($() -> "%28";
encode_char($)) -> "%29";
encode_char($*) -> "%2A";
encode_char($+) -> "%2B"; % should this always be encoded?
encode_char($,) -> "%2C";
encode_char($/) -> "%2F";
encode_char($:) -> "%3A";
encode_char($;) -> "%3B";
encode_char($=) -> "%3D";
encode_char($@) -> "%40";
encode_char($?) -> "%3F";
encode_char($[) -> "%5B";
encode_char($]) -> "%5D".
% No other characters are valid

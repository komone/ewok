%% UBF(A) specification by Joe Armstrong. 
%% SEE: http://www.sics.se/~joe/ubf/site/home.html
-module(ewok_ubf).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("../include/ewok.hrl").

-export([encode/1, decode/1]).
-export([int/0, constant/0, string/0, bin/0]).

%% API

%% UBF(A)
%% Codec
encode(Term) ->
	list_to_binary([encode_value(Term), <<$$>>]).
%%
decode(UBF) when is_list(UBF) ->
	decode(list_to_binary(UBF));
decode(UBF) ->
	{ok, [Term], <<>>} = decode_value(UBF, []),
	Term.

%% UBF(B) 
%% Fundamental types
int()      -> integer.
constant() -> constant.
string()   -> string.
bin()      -> binary.

%% UBF(C)
% All client/server remote-proceedure calls have the following form:
% Msg$  => {Reply, NextState}$
% Where Msg and Reply are UBF(A) types; NextState is the next state of the server.

%%
%% Internal functions
%%

%% UBF ENCODER
encode_value(Value) when is_atom(Value) ->
	[<<$'>>, atom_to_binary(Value, utf8), <<$'>>];
encode_value(Value) when is_integer(Value) ->
	list_to_binary(integer_to_list(Value));
encode_value(Value) when is_binary(Value) ->
	[encode_value(size(Value)), <<$~>>, Value, <<$~>>];
encode_value(Value) when is_tuple(Value) ->
	[<<${>>, encode_struct(tuple_to_list(Value), []), <<$}>>];
encode_value(Value) when is_list(Value) ->
	case Value =/= [] andalso is_printable(Value) of
	true -> [<<$">>, list_to_binary(Value), <<$">>];
	false -> encode_list(Value, [])
	end;
encode_value(Value) ->
	Value.
%%
encode_struct([H|T], Acc = []) ->
	encode_struct(T, [encode_value(H)| Acc]);
encode_struct([H|T], Acc) ->
	encode_struct(T, [encode_value(H), <<$,>>| Acc]);
encode_struct([], Acc) ->
	lists:reverse(Acc).
%%
encode_list([H|T], Acc) ->
	encode_list(T, [encode_value(H), <<$&>> | Acc]);
encode_list([], []) ->
	[<<$#, $&>>];
encode_list([], Acc) ->
	[<<$#>>, Acc].

%% TODO: find a function in the platform that determines "printable chars"
%% Only latin1 ubf "strings" are supported, non latin1 strings will be
%% encoded as lists.
is_printable([H|T]) ->
	case is_integer(H) 
		andalso ((H >= 32 andalso H < 127)
		orelse (H >= 160 andalso H < 256)) of
	true -> is_printable(T);
	false -> false
	end;		
is_printable([]) -> 
	true.

%% UBF DECODER
decode_value(<<$$, More/binary>>, []) ->
	{ok, [[]], More};
decode_value(<<$$, More/binary>>, Acc) ->
	{ok, Acc, More};
%% Comments
decode_value(<<$%, More/binary>>, Acc) ->
	{_, Rest} = read_binary(More, <<$%>>, <<>>),
	decode_value(Rest, Acc);
%% Constants
decode_value(<<$', More/binary>>, Acc) ->
	{Object, Rest} = read_binary(More, <<$'>>, <<>>),
	decode_value(Rest, [binary_to_atom(Object, utf8)|Acc]);
%% Binaries
decode_value(<<$~, More/binary>>, [Size|Acc]) ->
	{Object, Rest} = read_binary(More, <<$~>>, <<>>),
	Size = size(Object),
	decode_value(Rest, [Object|Acc]);
%% Strings
decode_value(<<$\", More/binary>>, Acc) ->
	{String, Rest} = read_binary(More, <<$\">>, <<>>),
	decode_value(Rest, [binary_to_list(String)|Acc]);
%% Tags => {Value:term(), Tag:string()}
decode_value(<<$`, More/binary>>, [Value|Acc]) ->
	{Tag, Rest} = read_binary(More, <<$`>>, <<>>),
	decode_value(Rest, [{Value, binary_to_list(Tag)}|Acc]);
%% Lists
decode_value(<<$#, $&, More/binary>>, Acc) ->
	decode_value(More, [[]|Acc]);
decode_value(<<$#, More/binary>>, Acc) ->
	decode_value(More, [[]|Acc]);
decode_value(<<$&, More/binary>>, [Value, List|Acc]) ->
	decode_value(More, [[Value|List]|Acc]);
%% Structs
decode_value(<<${, More/binary>>, Acc) ->
	decode_value(More, [{}|Acc]);
decode_value(<<$},More/binary>>, Acc) ->
	{Values, [{}|Tail]} = lists:splitwith(fun(X) -> X =/= {} end, Acc),
	Struct = list_to_tuple(lists:reverse(Values)),
	decode_value(More, [Struct|Tail]);
%% Whitespace
decode_value(<<$ , More/binary>>, Acc) ->
	decode_value(More, Acc);
decode_value(<<$\t, More/binary>>, Acc) ->
	decode_value(More, Acc);
decode_value(<<$\r, More/binary>>, Acc) ->
	decode_value(More, Acc);
decode_value(<<$\n, More/binary>>, Acc) ->
	decode_value(More, Acc);
decode_value(<<$,, More/binary>>, Acc) ->
	decode_value(More, Acc);
%% Integer or Other...
decode_value(Bin = <<Byte:1/binary, More/binary>>, Acc) ->
	case read_integer(Bin, <<>>) of
	{Value, Rest} -> decode_value(Rest, [Value|Acc]);
	_ -> decode_value(More, [Byte|Acc])
	end.

%%
read_binary(<<Terminal:1/binary, Rest/binary>>, Terminal, Acc) ->
	{Acc, Rest};
read_binary(<<Value:1/binary, Rest/binary>>, Terminal, Acc) ->
	read_binary(Rest, Terminal, <<Acc/binary, Value/binary>>).
%%
read_integer(Bin = <<Value:8/integer, Rest/binary>>, Acc) ->
	case Value of
	$- when Acc =:= <<>> -> 
		read_integer(Rest, <<$->>);
	_ when (Value >= $0 andalso Value =< $9) ->
		read_integer(Rest, <<Acc/binary, Value/integer>>);
	_ when Acc =/= <<>> -> 
		{list_to_integer(binary_to_list(Acc)), Bin};
	_ -> 
		false
	end.	

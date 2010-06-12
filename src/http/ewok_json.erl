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

-module(ewok_json).
-include("ewok.hrl").
-export([encode/1, decode/1]).
-compile(export_all).

%% @def RFC-4627

%%
encode(Term) ->
	encode_value(Term).	

%%
decode(JSON) ->
	Bin = strip(JSON),
	{Term, <<>>} = decode_value(Bin),
	Term.

%%
%% Internal functions
%%

%%
strip(Bin) ->
	strip(Bin, [], false).
%%
strip(<<$", Rest/binary>>, Acc, false) ->
	strip(Rest, [$"|Acc], true);
strip(<<$\\, $", Rest/binary>>, Acc, true) ->
	strip(Rest, [$", $\\|Acc], true);
%% pre-expand json unicode escapes
strip(<<$\\, $u, Hex:4/binary, Rest/binary>>, Acc, true) ->
	Value = ewok_hex:decode(Hex),
	strip(Rest, [Value|Acc], true);
strip(<<$", Rest/binary>>, Acc, true) ->
	strip(Rest, [$"|Acc], false);
strip(<<$ , Rest/binary>>, Acc, false) ->
	strip(Rest, Acc, false);
strip(<<$\t, Rest/binary>>, Acc, false) ->
	strip(Rest, Acc, false);
strip(<<$\r, Rest/binary>>, Acc, false) ->
	strip(Rest, Acc, false);
strip(<<$\n, Rest/binary>>, Acc, false) ->
	strip(Rest, Acc, false);
strip(<<X, Rest/binary>>, Acc, State) ->
	strip(Rest, [X|Acc], State);
strip(<<>>, Acc, false) ->
	list_to_binary(lists:reverse(Acc)).

%%
encode_value(true) ->
	<<"true">>;
encode_value(false) ->
	<<"false">>;
encode_value(null) ->
	<<"null">>;
encode_value(V) when is_binary(V) ->
	encode_string(V);
encode_value(V) when is_list(V) ->
	encode_array(V, []);
encode_value({json, V}) when is_list(V) ->
	encode_object(V, []);
encode_value(V) when is_integer(V); is_float(V) ->
	ewok_text:encode(V).
%%
encode_object([], []) ->
	<<"{}">>;
encode_object([{K, V}|[]], Acc) ->
	Pair = encode_pair(K, V),
	Bin = list_to_binary(lists:reverse([Pair|Acc])),
	<<"{", Bin/binary, "}">>;
encode_object([{K, V}|T], Acc) ->
	Pair = encode_pair(K, V),
	encode_object(T, [$,, Pair|Acc]).
%%
encode_pair(K, V) ->
	Key = encode_string(K),
	Value = encode_value(V),
	<<Key/binary, $:, Value/binary>>.
%%
encode_string(K) when is_atom(K) ->
	encode_string(atom_to_binary(K, utf8));
encode_string(K) when is_list(K) ->
	encode_string(list_to_binary(K));
encode_string(K) when is_binary(K) ->
	<<$", K/binary, $">>.
%%
encode_array([], []) ->
	<<"[]">>;
encode_array([V|[]], Acc) ->
	Value = encode_value(V),
	Bin = list_to_binary(lists:reverse([Value|Acc])),
	<<"[", Bin/binary, "]">>;
encode_array([V|T], Acc) ->
	Value = encode_value(V),
	encode_array(T, [$,, Value|Acc]).

%%
% json keywords
decode_value(<<"true", Bin/binary>>) ->
	{true, Bin};
decode_value(<<"false", Bin/binary>>) ->
	{false, Bin};
decode_value(<<"null", Bin/binary>>) ->
	{null, Bin};
% json object
decode_value(<<${, $}, Bin/binary>>) ->
	{{json, []}, Bin};
decode_value(<<${, Bin/binary>>) ->
	{Pairs, <<$}, Bin2/binary>>} = decode_object(Bin, []),
	{{json, Pairs}, Bin2};
% json array
decode_value(<<$[, $], Bin/binary>>) ->
	{[], Bin};
decode_value(<<$[, Bin/binary>>) ->
	{Values, <<$], Bin2/binary>>} = decode_array(Bin, []),
	{Values, Bin2};
% json string
decode_value(<<$", Bin/binary>>) ->
	{String, <<$", Bin2/binary>>} = decode_string(Bin, []),
	{String, Bin2};
% json number
decode_value(Bin) ->
	case re:run(Bin, <<"^-?[0-9]+">>) of
	{match, [{0, Length}]} ->
		<<X:Length/binary, Rest/binary>> = Bin,
		case re:run(Rest, <<"^\\.[0-9]+([Ee][+-]?[0-9]+)?">>) of
		nomatch ->
			Integer = list_to_integer(binary_to_list(X)),
			{Integer, Rest};
		{match, [{0, Length2}|_]} ->
			Total = Length + Length2,
			<<F:Total/binary, Rest2/binary>> = Bin,
			Float = list_to_float(binary_to_list(F)),
			{Float, Rest2}
		end
	end.
%%
decode_object(Bin, Acc) ->
	{Key, <<$:, Rest/binary>>} = decode_value(Bin),
	Key1 = decode_atom(Key),
	case decode_value(Rest) of
	{Value, <<$,, Rest2/binary>>} ->
		decode_object(Rest2, [{Key1, Value}|Acc]);
	{Value, <<Rest2/binary>>} ->
		{lists:reverse([{Key1, Value}|Acc]), Rest2}
	end.
%%
decode_array(Bin, Acc) ->
	case decode_value(Bin) of
	{Value, <<$,, Rest/binary>>} ->
		decode_array(Rest, [Value|Acc]);
	{Value, <<Rest/binary>>} ->
		{lists:reverse([Value|Acc]), Rest}
	end.
%%
decode_atom(Bin) ->
	try 
		binary_to_existing_atom(Bin, utf8)
	catch
	error:badarg ->
		Bin
	end.
%%
decode_string(Bin, _Acc) ->
	Length = seek(Bin, $"),
	<<String:Length/binary, Bin2/binary>> = Bin,
	{String, Bin2}.	
	
%% @credit Bob Ippolito
seek(Bin, X) ->
	seek(Bin, 0, X).
seek(Bin, Cursor, X) ->
	case Bin of
	<<_:Cursor/binary, $\\, X, _/binary>> ->
		seek(Bin, Cursor + 2, X);
	<<_:Cursor/binary, X, _/binary>> ->
		Cursor;
	<<_:Cursor/binary, _, _/binary>> ->
		seek(Bin, Cursor + 1, X)
	end.

%%%% Copyright 2009 Steve Davis <steve@simulacity.com>
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

%% NOTE: Because I can never remember how to use xmerl
%% TODO: Improve scope/validation
-module(ewok_xml).
-include("ewok.hrl").

-export([decode/1, encode/1]).
-compile(export_all).
-define(XML_REGEX, <<"[\t\n\r]|(<[^>]*>)">>).
-define(XML_DECL, <<"<?xml version=\"1.0\"?>">>).

%%
decode(File) when ?is_string(File) ->
	{ok, Bin} = file:read_file(File),
	decode(Bin);
decode(Bin) when is_binary(Bin) ->
	try
		[?XML_DECL|List] = split(Bin, ?XML_REGEX),
		{Root, []} = decode_elements(List, [], []),
		{xml, [], Root}
	catch
	E:R -> {E, R}
	end.

%%
encode({xml, [], Markup}) ->
	encode(Markup);
encode(Term) -> 
	try
		list_to_binary([?XML_DECL, encode_element(Term)])
	catch
	E:R -> {E, R}
	end.

%%
decode_elements([Terminal|T], Terminal, Acc) ->
	{lists:reverse(Acc), T};
decode_elements([<<$<, Bin/binary>>|T], Terminal, Acc) ->
	[Tag, Close] = split(Bin, <<"(>|/>)">>, 2),
	[Name|Attrs] = split(Tag, <<" ">>),
	Pairs = decode_attrs(Attrs, []),
	case Close of
	<<"/>">> -> 
		decode_elements(T, Terminal, [{Name, Pairs, []}|Acc]);
	<<">">> ->
		ChildTerminal = <<"</", Name/binary, ">">>,
%		io:format("T: ~p~n", [Terminal]),
%		{BodyList, [Terminal|T1]} = lists:splitwith(fun(X) -> X =/= Terminal end, T),
		{Body, Rest} = decode_elements(T, ChildTerminal, []),
		decode_elements(Rest, Terminal, [{Name, Pairs, Body}|Acc])
	end;
decode_elements([H|T], Terminal, Acc) ->
	decode_elements(T, Terminal, [H|Acc]);
decode_elements([], _Terminal, Acc) ->
	{lists:reverse(Acc), []}.
%
decode_attrs([H|T], Acc) ->
	[Name, Value] = split(H, <<$=>>),
	decode_attrs(T, [{make_key(Name), ewok_util:unquote(Value)}|Acc]);
decode_attrs([], Acc) -> 
	lists:reverse(Acc).
	

%%
encode_element({Name, Attrs, Content}) ->
	StartTag = [$<, makeio(Name), encode_attrs(Attrs, [])],
	Element = 
		case encode_content(Content, []) of
		[] -> [StartTag, $/, $>];
		Value -> [StartTag, $>, Value, $<, $/, makeio(Name), $>]
		end,
	list_to_binary(Element);
encode_element(Content) ->
	makeio(Content).
%%
encode_attrs([{K, V}|T], Acc) ->
	Attr = list_to_binary([<<$ >>, makeio(K), <<$=, $">>, makeio(V), <<$">>]),
	encode_attrs(T, [Attr|Acc]);
encode_attrs([], Acc) ->
	lists:reverse(Acc).
%%
encode_content([H|T], Acc) ->
	encode_content(T, [encode_element(H)|Acc]);
encode_content([], Acc) ->
	lists:reverse(Acc).


%% "clean" splitter
split(Bin, Regex) ->
	split(Bin, Regex, []).
split(Bin, Regex, Parts) when is_integer(Parts) ->
	split(Bin, Regex, [{parts, Parts}]);
split(Bin, Regex, Opts) ->
	[X || X <- re:split(Bin, Regex, Opts), X =/= <<>>].

%%
make_key(Name) ->
	try 
		binary_to_existing_atom(Name, utf8) 
	catch
	_:_ -> 
		Name
	end.

%%
makeio(X) when is_integer(X) -> makeio(integer_to_list(X));
makeio(X) when is_float(X)   -> makeio(float_to_list(X));
makeio(X) when is_atom(X)    -> atom_to_binary(X, utf8);
makeio(X) when is_list(X)    -> list_to_binary(X);
makeio(X) when is_binary(X)  -> X.

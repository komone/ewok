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

-export([decode/1, encode/1, xpath/2]).

-define(XML_REGEX, <<"[\t\n\r]|(<[^>]*>)">>).
-define(XML_DECL, <<"<?xml version=\"1.0\"?>">>).

%%
decode(File) when ?is_string(File) ->
	{ok, Bin} = file:read_file(File),
	decode(Bin);
decode(Bin) when is_binary(Bin) ->
	try
		[?XML_DECL|List] = ewok_util:split(Bin, ?XML_REGEX),
		{Root, []} = decode_elements(List, [], []),
		{xml, [{version, {1, 0}}], Root}
	catch
	E:R -> {E, R}
	end.
%%
encode({xml, [{version, 1}], Markup}) ->
	encode(Markup);
encode([Term]) -> 
	try
		Result = encode_element(Term),
		list_to_binary([?XML_DECL, Result])
	catch
	E:R -> {E, R}
	end.
	
%%
xpath(Xpath, {xml, _, Markup}) ->
	Path = ewok_util:split(Xpath, <<"/">>),
%	?TTY({path, Path}),
	path(Path, Markup).
%%
path([H, <<"values()">>], Values = [{H, _Attrs, _Body}|_Rest]) ->
	[X || {_, _, X} <- Values];
path([H, <<"node()">>], [Element = {H, _Attrs, _Body}|_Rest]) ->
	Element;
path([H], [{H, _Attrs, Body}|_Rest]) ->
%	?TTY({found, H}),
	Body;
path([H, <<"text()">>], [{H, _Attrs, Body}|_Rest]) ->
	Body;
path([H|T], [{H, _Attrs, Body}|_Rest]) ->
%	?TTY({traverse, H}),
	path(T, Body);
path(Path, [{_H, _, _}|Rest]) ->
%	?TTY({ignore, Path, H}),
	path(Path, Rest);
path([_|_], _) ->
	[];
path([], _) ->
	[].


%% @private

%%
decode_elements([Terminal|T], Terminal, Acc) ->
	{lists:reverse(Acc), T};
decode_elements([<<$<, Bin/binary>>|T], Terminal, Acc) ->
	[Tag, Close] = ewok_util:split(Bin, <<"(>|/>)">>, 2),
	[Name|Attrs] = ewok_util:split(Tag, <<" ">>),
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
	[Name, Value] = ewok_util:split(H, <<$=>>),
	decode_attrs(T, [{make_key(Name), ewok_util:unquote(Value)}|Acc]);
decode_attrs([], Acc) -> 
	lists:reverse(Acc).

%%
encode_element({Name, Content}) ->
	encode_element({Name, [], Content});
encode_element({Name, Attrs, Content}) ->
	%?TTY({encode, Name}),
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

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

%% NOTE: Because I can never remember how to use xmerl and do not need
%% a full DOM representation in 90% of practical circumstances for this 
%% document markup language which has found its way, incorrectly, into 
%% data processing.
%% TODO: Improve scope/validation
-module(ewok_xml).
-include("ewok.hrl").

-export([decode/1, encode/1, xpath/2]).

-define(XML_REGEX, <<"[\t\n\r]|(<[^>]*>)">>).
-define(XML_DECL, <<"<?xml version=\"1.0\"?>">>).

%% unused
% -record(element, {tag, attrs, body}).

%%
decode(File) when ?is_string(File) ->
	{ok, Bin} = file:read_file(File),
	decode(Bin);
decode(Bin) when is_binary(Bin) ->
%	try
	[_|List] = ewok_text:split(Bin, ?XML_REGEX),
	{Root, []} = decode_elements(List, [], []),
	{xml, [{version, {1, 0}}], Root}.
%	catch
%	E:R -> {E, R}
%	end.
%%
encode({xml, [{version, 1}], Markup}) ->
	encode(Markup);
encode([Term]) -> 
	try
		Result = encode_element(Term),
		list_to_binary([?XML_DECL, $\n, Result])
	catch
	E:R -> {E, R}
	end.
	
%%
xpath(Xpath, {xml, _, Markup}) ->
	Path = ewok_text:split(Xpath, <<"/">>),
%	?TTY({path, Path}),
	parse_path(Path, Markup).
%%
parse_path([H, <<"values()">>], Values = [{H, _Attrs, _Body}|_Rest]) ->
	[X || {_, _, X} <- Values];
parse_path([H, <<"node()">>], [Element = {H, _Attrs, _Body}|_Rest]) ->
	Element;
parse_path([H, <<"text()">>], [{H, _Attrs, Body}|_Rest]) ->
	Body;
parse_path([H], [{H, _Attrs, Body}|_Rest]) ->
%	?TTY({found, H}),
	Body;
parse_path([H|T], [{H, _Attrs, Body}|_Rest]) ->
%	?TTY({traverse, H}),
	parse_path(T, Body);
parse_path(Path, [{_H, _, _}|Rest]) ->
%	?TTY({ignore, Path, H}),
	parse_path(Path, Rest);
parse_path([_|_], _) ->
	[];
parse_path([], _) ->
	[].


%%
decode_elements([Terminal|T], Terminal, Acc) ->
	{lists:reverse(Acc), T};
decode_elements([<<$<, Bin/binary>>|T], Terminal, Acc) ->
	[Tag, Close] = ewok_text:split(Bin, <<"(>|/>)">>, 2),
	%% TODO: clean this up
%	[Name|Attrs] = ewok_text:split(Tag, <<" ">>),
	R = ewok_text:split(Tag, <<"([0-9A-Za-z:]*=\"[^\"]+\")">>),
	R1 = [ewok_text:trim(X) || X <- R],
	R2 = [X || X <- R1, X =/= <<>>],
	[Name|Attrs] = R2,
	Pairs = decode_attrs(Attrs, []),
	case Close of
	<<"/>">> -> 
		decode_elements(T, Terminal, [{make_key(Name), Pairs, []}|Acc]);
	<<">">> ->
		ChildTerminal = <<"</", Name/binary, ">">>,
%		io:format("T: ~p~n", [Terminal]),
%		{BodyList, [Terminal|T1]} = lists:splitwith(fun(X) -> X =/= Terminal end, T),
		{Body, Rest} = decode_elements(T, ChildTerminal, []),
		decode_elements(Rest, Terminal, [{make_key(Name), Pairs, Body}|Acc])
	end;
decode_elements([H|T], Terminal, Acc) ->
	case ewok_text:trim(H) of %% remove whitespace -- make optional?
	<<>> ->
		decode_elements(T, Terminal, Acc);
	Value ->
		decode_elements(T, Terminal, [make_key(Value)|Acc])
	end;
decode_elements([], _Terminal, Acc) ->
	{lists:reverse(Acc), []}.
%%
decode_attrs([H|T], Acc) ->
	[Name, Value] = ewok_text:split(H, <<$=>>),
	decode_attrs(T, [{make_key(Name), ewok_text:unquote(Value)}|Acc]);
decode_attrs([], Acc) -> 
	lists:reverse(Acc).

%%
encode_element({Name, Content}) ->
	encode_element({Name, [], Content});
encode_element({Name, Attrs, Content}) ->
%	?TTY({encode, Name}),
	StartTag = [$\n, $<, ewok_text:encode(Name), encode_attrs(Attrs, [])], %% readability \n
	Element = 
		case encode_content(Content, []) of
		[] -> 
			[StartTag, $/, $>, $\n]; %% readability \n
		Value -> 
			[StartTag, $>, Value, $<, $/, ewok_text:encode(Name), $>, $\n] %% readability \n
		end,
	list_to_binary(Element);
encode_element(Content) ->
	ewok_text:encode(Content).
%%
encode_attrs([{K, V}|T], Acc) ->
	Attr = list_to_binary([<<$ >>, ewok_text:encode(K), <<$=, $">>, ewok_text:encode(V), <<$">>]),
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


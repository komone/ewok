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
-module(ewok_xml).
-include("ewok.hrl").

-export([decode/1, encode/1, xpath/2, format/1]).

-define(XML_REGEX, <<"[\t\n\r]|(<[^>]*>)">>).
-define(XML_DECL, <<"<?xml version=\"1.0\"?>">>).

%% unused
% -record(element, {tag, attrs, body}).

%% TODO: Improve scope, add validation

	
%%
xpath(Xpath, {xml, _, Markup}) ->
	Path = ewok_text:split(Xpath, <<"/">>),
	Path0 = [make_key(X) || X <- Path],
%	?TTY({path, Path0}),
	parse_path(Path0, Markup).
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
decode(File) when ?is_string(File) ->
	{ok, Bin} = file:read_file(File),
	decode(Bin);
decode([Bin]) when is_binary(Bin) ->
	decode(Bin);
decode(Bin) when is_binary(Bin) ->
%	try
	List = ewok_text:split(Bin, ?XML_REGEX),
	{Root, []} = decode_elements(List, [], []),
	{xml, [{version, {1, 0}}], Root}.
%	catch
%	E:R -> {E, R}
%	end.
%%
decode_elements([Terminal|T], Terminal, Acc) ->
	{lists:reverse(Acc), T};
decode_elements([<<$<, Bin/binary>>|T], Terminal, Acc) ->
	[Tag, Close] = ewok_text:split(Bin, <<"(>|/>)">>, 2),
	%% TODO: clean this up
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
encode_element({Name, Content}) ->
	encode_element({Name, [], Content});
encode_element({{NS, Name}, Attrs, Content}) ->
	Prefix = ewok_text:encode(NS),
	Suffix = ewok_text:encode(Name),
	encode_element({list_to_binary([Prefix, $:, Suffix]), Attrs, Content});
encode_element({Name, Attrs, Content}) ->
%	?TTY({encode, Name}),
	StartTag = [$<, ewok_text:encode(Name), encode_attrs(Attrs, [])],
	Element = 
		case encode_content(Content, []) of
		[] -> 
			[StartTag, $/, $>];
		Value -> 
			[StartTag, $>, Value, $<, $/, ewok_text:encode(Name), $>]
		end,
	list_to_binary(Element);
encode_element(Content) ->
	ewok_text:encode(Content).
%%
encode_attrs([{K, V}|T], Acc) ->
	case K of
	{NS, Name} ->
		Prefix = ewok_text:encode(NS),
		Suffix = ewok_text:encode(Name),
		K0 = list_to_binary([Prefix, $:, Suffix]);
	_ ->
		K0 = ewok_text:encode(K)
	end,
	Attr = list_to_binary([<<$ >>, K0, <<$=, $">>, ewok_text:encode(V), <<$">>]),
	encode_attrs(T, [Attr|Acc]);
encode_attrs([], Acc) ->
	lists:reverse(Acc).
%%
encode_content([H|T], Acc) ->
	encode_content(T, [encode_element(H)|Acc]);
encode_content([], Acc) ->
	lists:reverse(Acc).

%% xmlns
make_key(Name) ->
	case ewok_text:split(Name, <<$:>>) of
	[Prefix, Key] ->
		{make_atom(Prefix), make_atom(Key)};
	[Key] ->
		make_atom(Key)
	end.

make_atom(Name) ->
	try 
		binary_to_existing_atom(Name, utf8) 
	catch
	_:_ -> 
		Name
	end.
%%
format(Xml) ->
	try
		List = ewok_text:split(Xml, ?XML_REGEX),
		format(List, 0, [])
	catch 
	_:_ ->
		Xml
	end.
%	
format([H = <<"<!", _/binary>>|T], Indent, Acc) ->
	E = list_to_binary([tabs(Indent), H, <<"\n">>]),
	format(T, Indent, [E|Acc]);
format([H = <<"<?", _/binary>>|T], Indent, Acc) ->
	E = list_to_binary([tabs(Indent), H, <<"\n">>]),
	format(T, Indent, [E|Acc]);
format([H = <<"</", _/binary>>|T], Indent, Acc) ->
	E = list_to_binary([tabs(Indent - 1), H, <<"\n">>]),
	format(T, Indent - 1, [E|Acc]);
format([H = <<$<, _/binary>>|T], Indent, Acc) ->
	E = list_to_binary([tabs(Indent), H, <<"\n">>]),
	Size = size(H) - 2,
	case H of 
	<<_:Size/binary, "/>">> ->
		format(T, Indent, [E|Acc]);	
	_ ->
		format(T, Indent + 1, [E|Acc])	
	end;
format([H|T], Indent, Acc) ->
	E = list_to_binary([tabs(Indent), H, <<"\n">>]),
	format(T, Indent, [E|Acc]);	
format([], _, Acc) ->
	list_to_binary(lists:reverse(Acc)).
	

tabs(Indent) ->
	binary:copy(<<"    ">>, Indent).
	

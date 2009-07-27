-module(esp_src2html).

-export([render/1, file/1]).

-define(TOKENIZE, "([{}\(\)\\[\\]:, \\.\"\t\r\n])").

file(File) ->
	case filelib:is_regular(File) of
	true -> 
		{ok, Source} = file:read_file(File),
		Html = render(Source),
		file:write_file(filename:basename(File) ++ ".html", Html);
	false -> {error, not_found}
	end.

render(Source) when is_binary(Source); is_list(Source) ->
	Tokens = re:split(Source, ?TOKENIZE, [{return, list}]),
	%io:format("~p~n", [Tokens]),
	Markup = to_html(Tokens, []),
	Html = [
		"<html>\n<head>\n", css(), "\n</head>\n<body>\n",
		Markup,
		"<br/><br/>\n</body>\n</html>\n"
	],
	Html.
	
to_html([[]|T], Acc) -> to_html(T, Acc);
to_html([H = [$%|_]|T], Acc) -> 
	{Tokens, Rest} = get_comment([H|T], []),
	to_html(Rest, [["<span class=\"comment\">", Tokens, "</span>"]|Acc]);
to_html([M, ":"|T], Acc) -> 
	to_html(T, [["<span class=\"module\">", M, "</span>:"]|Acc]);
to_html([" "|T], Acc) -> to_html(T, [160|Acc]);
to_html(["\r"|T], Acc) -> to_html(T, ["<br/>"|Acc]);
to_html(["\t"|T], Acc) -> to_html(T, [[160, 160, 160, 160]|Acc]);
to_html(["\""|T], Acc) ->
	{Tokens, Rest} = get_string(T, []),
	to_html(Rest, [["<span class=\"string\">\"", Tokens, "\"</span>"]|Acc]);
to_html([H|T], Acc) -> 
	case is_keyword(H) of
	true -> to_html(T, [["<span class=\"keyword\">", H, "</span>"]|Acc]);
	_ -> 
		case re:run(H, "[A-Z].*") of
		nomatch ->
			to_html(T, [H|Acc]);
		_ ->
			to_html(T, [["<span class=\"variable\">", H, "</span>"]|Acc])
		end
	end;
to_html([], Acc) ->
	lists:reverse(Acc).

get_comment(Rest = ["\r"|_], Acc) -> {lists:reverse(Acc), Rest};
get_comment(Rest = ["\n"|_], Acc) -> {lists:reverse(Acc), Rest};
get_comment([H|T], Acc) -> get_comment(T, [H|Acc]).

get_string(["\""|Rest], Acc) -> {lists:reverse(Acc), Rest};
get_string([H|T], Acc) -> get_string(T, [H|Acc]).

css() -> ["<style>
	body {
		margin:10px;
		font-family: \"Courier New\", Courier, monotype;
		font-size: 10pt;
	}
	.keyword { font-weight: bold; }
	.module { color: #535; }
	.variable { color: #557; }
	.string { color: #a70; }
	.comment { color: #999; font-style:italic; }
	</style>\n"].

is_keyword("-author") -> true;
is_keyword("-behavior") -> true;
is_keyword("-behaviour") -> true;
is_keyword("-compile") -> true;
is_keyword("-define") -> true;
is_keyword("-endif") -> true;
is_keyword("-export") -> true;
is_keyword("-ifdef") -> true;
is_keyword("-ifndef") -> true;
is_keyword("-import") -> true;
is_keyword("-include") -> true;
is_keyword("-include_lib") -> true;
is_keyword("-module") -> true;
is_keyword("-record") -> true;
is_keyword("-undef") -> true;
is_keyword("-vsn") -> true;

is_keyword("after") -> true;
is_keyword("and") -> true;
is_keyword("andalso") -> true;
is_keyword("band") -> true;
is_keyword("begin") -> true;
is_keyword("bnot") -> true;
is_keyword("bor") -> true;
is_keyword("bsl") -> true;
is_keyword("bsr") -> true;
is_keyword("bxor") -> true;
is_keyword("case") -> true;
is_keyword("catch") -> true;
is_keyword("cond") -> true;
is_keyword("div") -> true;
is_keyword("end") -> true;
is_keyword("fun") -> true;
is_keyword("if") -> true;
is_keyword("let") -> true;
is_keyword("not") -> true;
is_keyword("of") -> true;
is_keyword("or") -> true;
is_keyword("orelse") -> true;
is_keyword("query") -> true;
is_keyword("receive") -> true;
is_keyword("rem") -> true;
is_keyword("try") -> true;
is_keyword("when") -> true;
is_keyword("xor") -> true;
is_keyword(_) -> false.




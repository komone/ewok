-module(esp_js).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-define(JS_FILE_EXT, ".js").

-export([minify/1, compile_ajax/2]).

%% TEMP(?) - Development Utility only - see: esp_ajax.erl
%% e.g. compile_ajax("myscript.js, ["id", "ctrl1", "ctrl2"]).
compile_ajax(File, Keywords) ->
	{ok, Bin} = file:read_file(File),
	Regex = make_ajax_regex(lists:reverse(Keywords), []),
	Term = [X || X = X1 <- re:split(Bin, Regex), X1 =/= <<>>],
	Matches = [list_to_binary(X) || X <- Keywords],
	F = fun(X) -> 
		case lists:member(X, Matches) of 
		true -> binary_to_existing_atom(X, utf8); 
		false -> X
		end
	end,
	Processed = [F(X) || X <- Term],
	Formatted = io_lib:format("~p~n", [Processed]),
	Filename = filename:absname(File ++ ".src"),
	ok = file:write_file(Filename, Formatted),
	{ok, Filename}.
%
make_ajax_regex([], Acc) -> 
	Whitespace = "[\t\r\n]",
	lists:flatten(lists:reverse([Whitespace|Acc]));
make_ajax_regex([H|T], Acc) ->
	make_ajax_regex(T, [[$(, H, $), $|]|Acc]).


%% jsmin in Erlang
%% <http://javascript.crockford.com/jsmin.html>
%% NOTE: The production version of JQuery.min.js is actually compressed/altered,
%% (by hand?) and includes http://sizzlejs.com/ - so this function won't generate a 
%% duplicate of the JQuery minified delivery. It will, however, duplicate the result
%% of using: jsmin <jquery-{version}.js >jquery-{version}.min.js
minify(File) when is_list(File) ->
	{ok, Bin} = file:read_file(File),
	Min = minify(Bin),
	Filename = filename:absname(filename:basename(File, ?JS_FILE_EXT) ++ ".min.js"),
	ok = file:write_file(Filename, Min),
	{ok, Filename};
minify(Bin) when is_binary(Bin) ->
	min_js(binary_to_list(Bin), []).

%% Replace // comments with LF
min_js([$/, $/|T], Acc) ->
	Rest = skip_to($\n, T),
	min_js([$\n|Rest], Acc);
%% Replace /* */ comments with a space
min_js([$/, $*|T], Acc) ->
	Rest = skip_to([$*, $/], T),
	min_js([$ |Rest], Acc);
%% Trap regex
min_js([$/|T], [Prev|Acc]) ->
	{Rest, Acc1} = 
		case is_js_regex(Prev) of
		true -> read_to($/, T, [$/, Prev|Acc]);
		false -> {T, [$/, Prev|Acc]}
		end,
	min_js(Rest, Acc1);
%% Trap double quoted strings...
min_js([$"|T], Acc) ->
	{Rest, Acc1} = read_to($", T, [$"|Acc]),
	min_js(Rest, Acc1);
%% Trap single-quoted strings...
min_js([$'|T], Acc) ->
	{Rest, Acc1} = read_to($', T, [$'|Acc]),
	min_js(Rest, Acc1);
%% Replace CR with LF
min_js([$\r|T], Acc) ->
	min_js([$\n|T], Acc);
%% Replace ctrl chars except LF, (but including TAB) with a space
%% NOTE: Assumes "ctrl chars" for ASCII cover all control chars
min_js([H|T], Acc) when H =:= 127 
		orelse (H < 32 andalso H =/= 10) -> 
	min_js([$ |T], Acc);
%% Reduce runs of spaces to one space
min_js([$ |T], Acc = [$ |_]) ->
	min_js(T, Acc);
%% Reduce runs of LF to one LF
min_js([$\n|T], Acc = [$\n|_]) ->
	min_js(T, Acc);	
%% Pre-Collapse whitespace
min_js([$\n, $ |T], Acc) ->
	min_js([$\n|T], Acc);
min_js([$\n, $\t|T], Acc) ->
	min_js([$\n|T], Acc);
min_js([$\n, $\r|T], Acc) ->
	min_js([$\n|T], Acc);
%% For compliance with Cockroft's jsmin.c implementation, trim any leading SPACE
min_js([$ |T], []) ->
	min_js(T, []);
%% For compliance with Cockroft's jsmin.c implementation, trim the trailing LF
min_js([$\n], Acc) ->
	min_js([], Acc);
%% Drop space when permissable
min_js([$ , Next|T], [Prev|Acc]) ->
	case is_omit_unsafe(Prev, $ , Next) of
	true -> min_js([Next|T], [$ , Prev|Acc]);
	false -> min_js([Next|T], [Prev|Acc])
	end;
%% Drop LF when permissable
min_js([$\n, Next|T], [Prev|Acc]) ->
	case is_omit_unsafe(Prev, $\n, Next) of
	true -> min_js([Next|T], [$\n, Prev|Acc]);
	false -> min_js([Next|T], [Prev|Acc])
	end;
%% Don't touch anything else
min_js([H|T], Acc) ->
	min_js(T, [H|Acc]);
min_js([], Acc) ->
	lists:reverse(Acc).

% found terminal char, return
skip_to(X, [X|T]) -> 
	T;
% found terminal chars, return
skip_to([X, Y], [X, Y|T]) -> 
	T;
% pass over everything else
skip_to(Match, [_H|T]) -> 
	skip_to(Match, T);
% error
skip_to(_, []) -> 
	throw("Unterminated Comment").

%% trap escapes
read_to(X, [$\\, H|T], Acc) -> 
	read_to(X, T, [H, $\\|Acc]);
% found terminal char, return
read_to(X, [X|T], Acc) -> 
	{T, [X|Acc]};
% pass through everything else
read_to(X, [H|T], Acc) -> 
	read_to(X, T, [H|Acc]);
% error
read_to(_, [], _Acc) ->	
	throw("Unterminated String").

%% Found / when previous non-ws char is one of:
%% ( ,  =  :  [  !  &  |  ?  {  }  ;  \n
is_js_regex(Prev) ->
	case re:run(<<Prev>>, "[\(,=:\[!&\|\?{};\n]") of
	{match, _} -> true;
	nomatch -> false
	end.

%% jsmin Spec: Omit space except when it is preceded and followed by a non-ASCII character 
%% or by an ASCII letter or digit, or by one of these characters: \ $ _
is_omit_unsafe(Prev, $ , Next) ->
	Regex = "[A-Za-z0-9_\\\\$]",
	is_match(Next, Regex) 
	andalso is_match(Prev, Regex);
%% jsmin Spec: Omit linefeed except:
%% if it follows a non-ASCII character or an ASCII letter or digit 
%% or one of these characters:  \ $ _ } ] ) + - " '
%% AND if it precedes a non-ASCII character or an ASCII letter or digit 
%% or one of these characters:  \ $ _ { [ ( + -
is_omit_unsafe(Prev, $\n, Next) ->
	(Prev =:= $" orelse Prev =:= $' 
		orelse is_match(Prev, "[A-Za-z0-9\\\\$_}\\]\)\+-]")) 
	andalso is_match(Next, "[A-Za-z0-9\\\\\$_{\[\(\+-]").
%%
is_match(X, Regex) ->
	case re:run(<<X>>, Regex) of
	{match, _} -> true;
	nomatch when X >= 128 -> true; % include non-ascii chars
	nomatch -> false
	end.

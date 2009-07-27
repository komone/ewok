-module(ewok_redoc).
-vsn("1.0").
-author('steve@simulacity.com').

-export([run/0]).

-record(ibeam, {mod, version, 'author', specs=[]}).
-record(ispec, {function, usage=unknown, spec, comment="undocumented"}).

run() ->
	run(ewok_util:appdir()).
	
run(AppPath) ->
	DocPath = filename:join(AppPath, "doc"),
	BeamPath = filename:join(AppPath, "ebin"),
	case filelib:is_dir(AppPath) of
	true ->
		case filelib:is_dir(DocPath) of 
		false -> ok = file:make_dir(DocPath);
		true -> ok
		end,
		BeamList = filelib:wildcard("*.beam", BeamPath),
		Html = generate_files(BeamList, DocPath, []),
		File = filename:join(DocPath, "api.html"),
		{file:write_file(File, Html), File};
	false ->
		{error, not_found}
	end.

generate_files([H|T], DocPath, Acc) ->
	Module = list_to_atom(filename:basename(H, ".beam")),
	Exports = lists:sort([X || X = {F, _} <- Module:module_info(exports), F =/= module_info]),
	Attrs = Module:module_info(attributes),
	Vsn = proplists:get_value('vsn', Attrs),
	Author = proplists:get_value('author', Attrs, ["author unknown"]),
	Interfaces = lists:append([X || {'interface', X} <- Attrs]),
	Specs = generate_specs(Module, Exports, Interfaces, []),
	Record = #ibeam{mod=Module, version=Vsn, 'author'=Author, specs=Specs},
	generate_files(T, DocPath, [Record|Acc]);
generate_files([], DocPath, Acc) ->
	to_html(DocPath, lists:reverse(Acc)).
	
generate_specs(Module, [H|T], Interfaces, Acc) ->
	Record = 
		case [X || X = {F, _, _, _} <- Interfaces, F =:= H] of
		[] -> #ispec{function=H};
		[{H, Usage, Comment, Spec}] -> #ispec{function=H, usage=Usage, spec=Spec, comment=Comment};
		_ -> #ispec{function=H, comment="ERROR: Invalid Interface Definition"}
		end,
	generate_specs(Module, T, Interfaces, [Record|Acc]);
generate_specs(_, [], _, Acc) ->
	lists:reverse(Acc).
	
to_html(DocPath, IList) -> [
		"<html>\n<head>\n<title>Ewok API</title>\n",
		css(), "</head>\n<body>\n",
		"<h1>Ewok/1.0 (Wicket) API</h1>\n",
		"<h2>Modules</h2>\n",
		"<table width=\"90%\" cellspacing=\"0\" cellpadding=\"4\" border=\"1\">\n<tr><td>",
		generate_module_index(IList, []),
		"</td></tr></table>",
		generate_html(IList, [], DocPath),
		"<hr>\n<p><em>Generated ", httpd_util:rfc1123_date(), "</em></p>\n"
		"</body>\n</html>\n"
	].

generate_module_index([H], Acc) when is_record(H, ibeam) ->
	Module = atom_to_list(H#ibeam.mod),
	Html = ["<a href=\"#", Module, "\">", Module, "</a>"],
	lists:reverse([Html|Acc]);
generate_module_index([H|T], Acc) when is_record(H, ibeam) ->
	Module = atom_to_list(H#ibeam.mod),
	Html = ["<a href=\"#", Module, "\">", Module, "</a>, "],
	generate_module_index(T, [Html|Acc]);
generate_module_index([], Acc) ->
	lists:reverse(Acc).
	
generate_html([M|T], Acc, DocPath) when is_record(M, ibeam) ->
	Module = io_lib:format("~p", [M#ibeam.mod]),
	Html = [
	"<a name=\"", Module, "\"/>\n",
	"<h3>", Module, "</h3>\n", 
	"<p><em><a href=\"../src/", Module, ".erl\">src</a> ",
	io_lib:format("~p version: ~p", [M#ibeam.'author', M#ibeam.version]), "</em></p>\n", 
	"<table width=\"90%\" cellspacing=\"0\" cellpadding=\"4\" border=\"1\">\n",
	"<thead><tr><th>Function</th><th>Returns</th><th>Usage</th><th>Comment</th></tr></thead>\n",
	"<tbody>\n",
	generate_html(M#ibeam.specs, [], DocPath),
	"</tbody></table>\n"
	],
	
	generate_html(T, [Html|Acc], DocPath);
generate_html([S|T], Acc, DocPath) when is_record(S, ispec) ->
	{F, Arity} = S#ispec.function,
	{Function, Returns} = 
		case S#ispec.spec of
		undefined ->
			{lists:append([atom_to_list(F), "/", integer_to_list(Arity)]), "?"};
		Value -> 
			[In, Out] = re:split(Value, "->", [{return, list}]),
			{lists:append(["<a href=\"#\">", atom_to_list(F), "</a>(", ewok_util:trim(In), ")"]), ewok_util:trim(Out)}
		end,
	Html = [ "<tr><td>", Function, "</td><td>", Returns, "</td><td>",
		atom_to_list(S#ispec.usage), "</td><td>", S#ispec.comment, "</td></tr>"],
	generate_html(T, [Html|Acc], DocPath);
generate_html([], Acc, _) ->
	lists:reverse(Acc).

css() ->
	["<link rel=\"stylesheet\" type=\"text/css\" href=\"api.css\"/>\n"].



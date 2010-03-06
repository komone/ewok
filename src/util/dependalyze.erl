-module(dependalyze).

-include("ewok.hrl").

-compile(export_all).

target() ->
	{ewok, [<<"ewok.hrl">>, <<"ewok_system.hrl">>], [
		{ewok_app, [
			{ewok_sup, [
				{ewok_db, []},
				{ewok_config, []}
			]},
			{ewok_config, []},
			{io, []}
		]}
	]}.
	
%%
application(App) when is_atom(App) ->
	application:load(App),
	{ok, {Module, _Args}} = application:get_key(App, mod),
	Path = ewok_file:path(code:which(Module)),
	{Headers, BeamFiles, _Parsed} = parse_files([Path], sets:new(), [], [], []),
%	{AHeaders, ABeamFiles, AParsed} = parse_files([Path], sets:new(), [], [], []),
%	All = ewok_file:list(ewok_file:parent(Path)),
%	Rest = [X || X <- All, lists:member(X, AParsed) =:= false],
%	{Headers, BeamFiles, _} = parse_files(Rest, AHeaders, ABeamFiles, [], AParsed),
	Result = {App, lists:sort(sets:to_list(Headers)), BeamFiles},
	Filename = ewok_file:path([ewok_util:appdir(ewok), <<"out.dep">>]),
	?TTY({file, Filename}),
	ok = ewok_file:save(Filename, io_lib:format("~p~n", [Result])).
	
%%
parse_files([H|T], Headers, BeamList, RootList, Parsed) when is_binary(H) ->
	case ewok_file:extension(H) of
	<<".beam">> ->
		Module = binary_to_atom(ewok_file:name(H), utf8),
		case lists:member(H, RootList) of
		true ->
			parse_files(T, Headers, [{Module, cyclic}|BeamList], RootList, Parsed);
		false ->
			Src = proplists:get_value(source, Module:module_info(compile)),
			%?TTY({source, Src}),
			case ewok_file:is_regular(Src) of
			true ->
				case lists:member(Src, Parsed) of
				true ->
					parse_files(T, Headers, [{Module, '+'}|BeamList], RootList, Parsed);
				false ->
					FileSet = lists:sort(parse_file(Src)),
					{ChildHeaders, Children, NewParsed} = parse_files(FileSet, sets:new(), [], [H|RootList], [Src|Parsed]),
					parse_files(T, sets:union(ChildHeaders, Headers), [{Module, Children}|BeamList], RootList, NewParsed)
				end;
			false ->
				parse_files(T, Headers, [{Module, '*'}|BeamList], RootList, Parsed)
			end
		end;
%		parse_files(T, Headers, [Value|BeamList], RootList, Parsed)
	<<".hrl">> ->
		parse_files(T, sets:add_element(H, Headers), BeamList, RootList, Parsed);
	<<".app">> ->
		parse_files(T, Headers, BeamList, RootList, Parsed)
	end;
parse_files([], Headers, BeamList, _RootList, Parsed) ->
	{Headers, lists:reverse(BeamList), Parsed}.

file(Path) ->
	{ok, Form} = epp:parse_file(Path, ["include"], []),
	DepFile = ewok_file:path([<<(ewok_file:name(Path))/binary, ".dep">>]),
	?TTY({file, DepFile}),
	file:write_file(binary_to_list(DepFile), io_lib:format("~p~n", [Form])).
	
%%
parse_file(Path) ->
	{ok, Form} = epp:parse_file(Path, ["include"], []),
	parse(Form).
	
%%
parse([{attribute, _, file, _}|Form]) ->
	Set = parse(Form, sets:new()),
	[X || X <- sets:to_list(Set)].
%%
parse([{attribute, 1, file, {File, _}}|T], Acc) ->
%	?TTY({header, File}),
	Path = ewok_file:path(File),
	parse(T, sets:add_element(Path, Acc));
parse([{attribute, _, _, _}|T], Acc) ->
	parse(T, Acc);
parse([{function, _, _Name, _, Body}|T], Acc) ->
	Inner = parse(Body, sets:new()),
	parse(T, sets:union(Inner, Acc));
parse([{clause, _, _, _, Body}|T], Acc) ->
	Inner = parse(Body, sets:new()),
	parse(T, sets:union(Inner, Acc));
parse([{cons, _Line, Expr, Body}|T], Acc) ->
	Cons = parse([Expr], sets:new()),
	Inner = parse([Body], sets:new()),
	parse(T, sets:union([Cons, Inner, Acc]));
parse([{'case', _Line, Expr, Body}|T], Acc) ->
	Case = parse([Expr], sets:new()),
	Inner = parse(Body, sets:new()),
	parse(T, sets:union([Case, Inner, Acc]));
parse([{match, _, _, Expr}|T], Acc) ->
	Inner = parse([Expr], sets:new()),
	parse(T, sets:union(Inner, Acc));
parse([{'fun', _, {clauses, Body}}|T], Acc) ->
	Inner = parse(Body, sets:new()),
	parse(T, sets:union(Inner, Acc));
parse([{tuple, _, Body}|T], Acc) ->
	Inner = parse(Body, sets:new()),
	parse(T, sets:union(Inner, Acc));
parse([{call, _, {remote, _, {atom, _, Name}, _}, Args}|T], Acc) ->
	Acc1 = parse_remote(Name, Args, Acc),
	Inner = parse(Args, sets:new()),
	parse(T, sets:union(Inner, Acc1));
parse([_H|T], Acc) ->
	parse(T, Acc);
parse([], Acc) ->
	Acc.

%
parse_remote(Name, Args, Acc) ->
	%?TTY({Name, Args}),
	case code:ensure_loaded(Name) of
	{'module', gen_server} ->
		case Args of
		[{tuple, _, _}, {atom, _, Module}|_] ->
			Path = ewok_file:path(code:which(Module)),
			sets:add_element(Path, Acc);
		[{atom, _, Module}|_] ->
			Path = ewok_file:path(code:which(Module)),
			sets:add_element(Path, Acc);
		_ ->
			Acc
		end;
	{'module', gen_fsm} ->
		case Args of
		[{tuple, _, _}, {atom, _, Module}|_] ->
			Path = ewok_file:path(code:which(Module)),
			sets:add_element(Path, Acc);
		[{atom, _, Module}|_] ->
			Path = ewok_file:path(code:which(Module)),
			sets:add_element(Path, Acc);
		_ ->
			Acc
		end;
	{'module', supervisor} ->
		case Args of
		[_, {atom, _, nil}|_] ->
			Acc;
		[_, {atom, _, Module}|_] ->
			Path = ewok_file:path(code:which(Module)),
			sets:add_element(Path, Acc);
		_ -> 
			Acc
		end;
	{'module', Name} ->
		case code:which(Name) of
		preloaded ->
			Acc;
		String ->
			Path = ewok_file:path(String),
			sets:add_element(Path, Acc)
		end
	end.

-module(ewok_file).

-include("../include/ewok.hrl").

-export([find/2, list/2]).
%% TEMP
-compile(export_all).
%% TEMP COPY: To allow compile only...
-record(file_cache, {route, path, mimetype, modified, size=0, bin=undefined}).
-record(esp_cache, {route, path, mimetype, modified, bin=undefined}).
-record(web_app, {id, path, valid=false, deployed=false}).

%% 
find(_Path, _Opts) ->
	ok.
	
%% 
list(_Path, _Opts) ->
	ok.
	
%% A dog's dinner: code:all_loaded()
%{io,"c:/ERLANG~2/lib/stdlib-1.16.2/ebin/io.beam"},
%{erl_distribution,"C:\\ERLANG~2/lib/kernel-2.13.2/ebin/erl_distribution.beam"},
%{edlin,"c:/ERLANG~2/lib/stdlib-1.16.2/ebin/edlin.beam"},
%{esp_html,"d:/Erlang/apps/ewok/ebin/esp_html.beam"},
%{dets_v9,"c:/ERLANG~2/lib/stdlib-1.16.2/ebin/dets_v9.beam"},
%{ewok_tcp_srv,"d:/Erlang/apps/ewok/ebin/ewok_tcp_srv.beam"},

%% collected crap

%% from utest
%%
find_files(Path, [$.|T]) ->
	Pattern = lists:flatten(["\\.", T, $$]),
	filelib:fold_files(Path, Pattern, true, fun(F, Acc) -> [F|Acc] end, []);
%
find_files(Path, Dir) when is_list(Dir) -> 
	FullPath = filename:join([Path, Dir]), 
	filelib:fold_files(FullPath, ".*", false, fun(F, Acc) -> [F|Acc] end, []).


%% full_path() -> #web_app{} | undefined
load_path(Path) when ?is_string(Path) ->
	case filelib:is_dir(Path) of 
	true -> 
		load_path1(Path);
	false ->
		case filelib:is_regular(Path) 
			andalso filename:extension(Path) =:= ?ARCHIVE_FILE_EXT of
		true ->
			load_path1(filename:join([Path, filename:basename(Path, ".ez")]));
		false -> 
			undefined
		end
	end.
%%
load_path1(Path) ->
	BeamDir = filename:join(Path, "ebin"),
	%?TTY("Trying: ~p~n", [Path]),
	case filelib:wildcard("*" ++ ?CONFIG_FILE_EXT, BeamDir) of
	[AppFile|_] ->
		%?TTY("AppFile: ~p~n", [{AppFile, BeamDir}]),
		AppName = list_to_atom(filename:basename(AppFile, ?CONFIG_FILE_EXT)),
		code:add_pathz(BeamDir),
		Valid = validate(AppName),
		#web_app{id=AppName, path=Path, valid=Valid};
	_ -> 
		undefined
	end.
	
%% stub
validate(_Name) -> ok.

%
read_file(Path) ->
	BasePath = 
		case ewok:config({ewok, http, www_root}, ?DEFAULT_WWW_ROOT) of
		Root = [$., $/|_] -> filename:absname(Root);
		Root = [$/|_] -> filename:absname([$.|Root])
		end,
	File = filename:join(BasePath, [$.|Path]), 
	case filelib:is_regular(File) of
	true -> 
		{ok, Bin} = file:read_file(File),
		%% TODO: NOTE: owing to index files two copies of indexes may be stored
		%% under different routes e.g. both /stuff and /stuff/index.html
		%% -- not sure what is the best solution to avoid this just yet...
		#esp_cache{
			route = Path,
			path = File,
			mimetype = ewok_http:mimetype(filename:extension(File)),
			modified = ewok_http:date(filelib:last_modified(File)),
			bin=Bin
		};
	false -> 
		undefined
	end.

load_template(undefined, Path) -> 
	{error, {undefined, Path}};
load_template(Dir, Path) ->
	File = filename:join([code:lib_dir(ewok), Dir, Path]),
	case filelib:is_regular(File) of
	true -> 
		{ok, Bin} = file:read_file(File),
		parse_template(Bin);
	false -> 
		{error, File}
	end.

%% stub
parse_template(_Bin) -> ok.

%%
load_termfile(App) ->
	case code:ensure_loaded(App) of
	{'module', App} ->
		Path = filename:dirname(code:which(App)),
		File = filename:join(Path, atom_to_list(App) ++ ?CONFIG_FILE_EXT),
		case filelib:is_regular(File) of
		true ->
			try
				{ok, Terms} = file:consult(File),
				{ok, File, Terms}
			catch
			%% C{error,{103,erl_parse,["syntax error before: ","'{'"]}}
			_:{badmatch, {error, {Line, _, Message}}} -> 
				{error, {file, File}, {line, Line}, {reason, lists:flatten(Message)}};
			E:R -> {E, R}
			end;
		false ->
			{error, {nofile, File}}
		end;
	_ -> {error, {no_app_found, App}}
	end.
	
%% request(), string() -> #file_cache{}
get_file(Request, Path) ->
	Realm = Request:realm(),
	BasePath = 
		case ewok:config({Realm, http, www_root}, ?DEFAULT_WWW_ROOT) of
		Root = [$., $/|_] ->
			filename:join(code:lib_dir(Realm), Root);
		Root = [$/|_] -> 
			filename:join(code:lib_dir(Realm), [$., Root])
		end,
	File = filename:join(BasePath, [$.|Path]), 
	case filelib:is_dir(File) of
	true ->
		Index = ewok:config({Realm, http, index_file}, "index.html"),
		get_file(Request, filename:join(Path, Index));
	false ->
		case filelib:is_regular(File) of
		true -> 
			%% TODO: NOTE: owing to index files two copies of indexes may be stored
			%% under different routes e.g. both /stuff and /stuff/index.html
			%% -- not sure what is the best solution to avoid this just yet...
			#file_cache{
				route = Request:path(),
				path = File, 
				mimetype = ewok_http:mimetype(filename:extension(File)),
				modified = ewok_http:date(filelib:last_modified(File)),
				size = filelib:file_size(File),
				bin=undefined
			};
		false -> 
			undefined
		end
	end.
	
keystore() ->
	Path = ewok:config({ewok, identity, keystore}, "./priv/data"),
	Dir = filename:join(code:lib_dir(ewok), Path),
	case filelib:is_dir(Dir) of
	true ->
		File = filename:join(Dir, ?KEYSTORE_FILE),
		case filelib:is_regular(File) of
		true ->	
			{ok, [Term]} = file:consult(File),
			Term;
		false ->
			{error, no_keystore}
		end;
	false ->
		{error, invalid_path}
	end.

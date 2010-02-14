%% Copyright 2009 Steve Davis <steve@simulacity.com>
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

-module(ewok_file).
-interface([
	{appname/1, api, "The application name if resolvable from the provided path", "binary() -> atom() | undefined"},
	{close/1, api, "Close the file", "binary() -> ok | {error, Term}"},
	{code_path/1, api, "The code directory if resolvable from the provided path", "binary() -> binary() | undefined"},
	{save/2, api, "Write contents to a file", "binary(), binary() -> ok | {error, Term}"},
	{extension/1, api, "The file extension from the provided path", "binary() -> binary()"},
	{file_info/1, api, "The file descriptor information", "binary() -> {ok, file_info()} | undefined"},
	{find/2, api, "A list of matching files", "binary(), binary() -> list()"},
	{find/3, api, "A list of matching files", "binary(), binary(), recursive -> list()"},
	{is_directory/1, api, "If the path refers to a directory", "binary() -> boolean()"},
	{is_file/1, api, "If the path refers to a regular file", "binary() -> boolean()"},
	{is_link/1, api, "If the path refers to a sym link", "binary() -> boolean()"},
	{is_regular/1, api, "If the path refers to a regular file", "binary() -> boolean()"},
	{list/1, api, "The list of files in the directory referred to by the path", "binary() -> list() | {error, Term}"},
	{load/1, api, "Read the file contents", "binary() -> binary() | undefined"},
	{name/1, api, "The file or directory name without an extension", "binary() -> binary()"},
	{open/2, api, "Open the file", "binary(), opts() -> {ok, fd()} | {error, Term}"},
	{parent/1, api, "An absolute path to the parent directory", "binary() -> binary()"},
	{path/1, api, "An absolute binary path from path components", "list() -> binary()"}
]).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/zip.hrl").

-export([open/2, close/1]).
-export([path/1, parent/1, name/1, appname/1, extension/1]).
-export([load/1, save/2, resource/2]). %, file_info/1]).
-export([list/1, code_path/1]). 
-export([is_directory/1, is_file/1, is_regular/1, is_link/1]).
-export([file_info/1, find/2, find/3]).

-define(PATH_SEPARATOR, <<"[\\\\/]">>).
-define(BIN_DIR, <<"ebin">>).

%% TODO: Refactoring target
%% BUG: <<"../..">> -> <<"..">>.
path(Bin) when is_binary(Bin) ->
	path([Bin]);
path(List = [H|_]) when is_integer(H) -> % "strings"
	path([list_to_binary(List)]);
path(List) ->
	{ok, CD} = file:get_cwd(),
	Filtered = filter_paths(List, [list_to_binary(CD)]),
	Parts = [ewok_util:split(X, ?PATH_SEPARATOR) || X <- Filtered],
%	?TTY("~p~n", [lists:append(Parts)]),
	make_path(lists:append(Parts), <<>>).

%% @private
%% convert "string" elements and drop elements that precede an absolute path
filter_paths([String = [H|_]|T], Acc) when is_integer(H) ->
	filter_paths([list_to_binary(String)|T], Acc);
filter_paths([H = <<$/, _/binary>>|T], _Acc) ->
	filter_paths(T, [H]);
filter_paths([Windoze = <<_, $:, _/binary>>|T], _Acc) ->
	filter_paths(T, [Windoze]);
filter_paths([H|T], Acc) ->
	filter_paths(T, [H|Acc]);
filter_paths([], Acc) ->
	lists:reverse(Acc).

%% @private
make_path([<<$.>>|T], <<>>) -> 
	{ok, CD} = file:get_cwd(),
	make_path(T, list_to_binary(CD));
% parent of cwd
make_path([<<"..">>|T], <<>>) -> %%
	{ok, Dir} = file:get_cwd(),
	CurrentDir = ewok_util:split(Dir, ?PATH_SEPARATOR),
	[_|Rest] = lists:reverse(CurrentDir),
	Base = make_path(lists:reverse(Rest), <<>>),
	make_path(T, Base);
% deal with windows drive labels
make_path([Volume = <<_, $:>>|T], <<>>) ->
	Acc = ewok_util:to_upper(Volume),
	make_path(T, Acc);
% current
make_path([<<$.>>, H|T], Acc) ->
	make_path(T, <<Acc/binary, $/, H/binary>>);
% parent
make_path([_H, <<"..">>|T], Acc) ->
	make_path(T, Acc);
%
make_path([H|T], Acc) when is_binary(H) ->
	make_path(T, <<Acc/binary, $/, H/binary>>);
%
make_path([], Acc) ->
	Acc.

%% TODO: test
code_path(Path) ->
	case is_regular(Path) of
	true ->
		case extension(Path) of
		<<".ez">> ->
			archive_code_path(Path);
		_ -> 
			Parent = ewok_file:parent(Path),
			case ewok_file:name(Parent) of
			?BIN_DIR ->
				Parent;
			_ -> 
				undefined
			end
		end;
	_ ->
		case is_directory(Path) andalso ewok_file:name(Path) of
		?BIN_DIR ->
			Path;
		_ ->
			BeamDir = path([Path, ?BIN_DIR]),
			case is_directory(BeamDir) of
			true -> BeamDir;
			false -> undefined
			end
		end
	end.
	
%% @private
archive_code_path(Archive) ->
	Name = name(Archive),
	InternalPath = <<Name/binary, $/, ?BIN_DIR/binary, $/>>,
	{ok, Entries} = zip:list_dir(binary_to_list(Archive)),
	EntryNames = [X || #zip_file{name=X} <- Entries],
	case lists:member(binary_to_list(InternalPath), EntryNames) of
	true ->
		path([Archive, InternalPath]);
	false ->
		undefined
	end.

%%
name(Path) ->
	File = lists:last(ewok_util:split(Path, ?PATH_SEPARATOR)),
	Ext = extension(File),
	case re:replace(File, <<Ext/binary, $$>>, <<>>) of
	Name when is_binary(Name) ->
		Name;
	[Name|_] ->
		Name
	end.

%%
parent(Path) ->
	path([Path, <<"..">>]).
	
%%
appname(CodePath) ->
	case name(CodePath) of
	?BIN_DIR ->
		Name = name(parent(CodePath)),
		[AppName|_Version] = ewok_util:split(Name, <<$->>),
		binary_to_atom(AppName, utf8);
	_ ->
		undefined
	end.

%%
extension(Path) ->
	File = lists:last(ewok_util:split(Path, ?PATH_SEPARATOR)),
	case lists:reverse(ewok_util:split(File, <<"(\\.)">>)) of
% NOTE: is this valid? i.e. are 'dot' files like .bashrc "hidden' or "anonymous" files?
%	[_Name, <<$.>>] ->
%		undefined; 
	[Ext, <<$.>>|_] -> 
		<<$., Ext/binary>>;
	_ -> 
		<<>>
	end.

%%
resource(File, Route) ->
	Info = file_info(File),
	#ewok_file{
		route = Route,
		file = File,
		mimetype = ewok_http:mimetype(extension(File)),
		modified = ewok_http:date(Info#file_info.mtime),
		size = Info#file_info.size,
		bin=undefined
	}.
%%
save(Path, Bin) ->
	file:write_file(binary_to_list(Path), Bin).
%%
load(Resource = #ewok_file{}) ->
	Bin = load(Resource#ewok_file.file),
	Resource#ewok_file{bin=Bin};
%
load(Path) when is_binary(Path) ->
	case is_regular(Path) of
	true ->	
		{ok, Bin} = file:read_file(binary_to_list(Path)),
		Bin;
	false ->
		undefined
	end.

%%
list(Path) when is_binary(Path) ->
	case is_directory(Path) of
	true ->
		{ok, Files} = file:list_dir(binary_to_list(Path)),
		[path([Path, X]) || X <- Files]; %, is_regular(X)];
	false ->
		{error, not_directory}
	end.

%%
is_file(File) ->
	is_regular(File).
%%
is_link(File) ->
	is_type(File, symlink).
%%
is_regular(File) -> 
	is_type(File, regular).
%%	
is_directory(File) ->
	is_type(File, directory).

%% @private
is_type(File, Type) ->
	case file_info(File) of
	Info = #file_info{} ->
		Info#file_info.type =:= Type;
	_ ->
		false
	end.
	
%%	
open(Path, Opts) ->
	file:open(binary_to_list(Path), Opts).

%%
close(Fd) ->
	file:close(Fd).
	
%%
file_info(File) when is_binary(File) ->
	file_info(binary_to_list(File));
%
file_info(File) ->
	case file:read_file_info(File) of
	{ok, Info} -> Info;
	_ -> undefined
	end.

%% 
find(BasePath, Regex) ->
	find(BasePath, Regex, false).
%
find(BasePath, Regex, recursive) ->	
	find(BasePath, Regex, true);
%
find(BasePath, Regex, Recurse) when is_binary(BasePath) ->
	PathString = binary_to_list(BasePath),
	Pred = fun(F, Acc) -> [path(F)|Acc] end,
	filelib:fold_files(PathString, Regex, Recurse, Pred, []).
	

%% COLLECTED CRAP

%% A dog's dinner: code:all_loaded()
%{io,"c:/ERLANG~2/lib/stdlib-1.16.2/ebin/io.beam"},
%{erl_distribution,"C:\\ERLANG~2/lib/kernel-2.13.2/ebin/erl_distribution.beam"},
%{edlin,"c:/ERLANG~2/lib/stdlib-1.16.2/ebin/edlin.beam"},
%{esp_html,"d:/Erlang/apps/ewok/ebin/esp_html.beam"},
%{dets_v9,"c:/ERLANG~2/lib/stdlib-1.16.2/ebin/dets_v9.beam"},
%{ewok_tcp_srv,"d:/Erlang/apps/ewok/ebin/ewok_tcp_srv.beam"},

%% full_path() -> #web_app{} | undefined
%xload_path(Path) when ?is_string(Path) ->
%	case filelib:is_dir(Path) of 
%	true -> 
%		load_path1(Path);
%	false ->
%		case filelib:is_regular(Path) 
%			andalso filename:extension(Path) =:= ?ARCHIVE_FILE_EXT of
%		true ->
%			load_path1(filename:join([Path, filename:basename(Path, ".ez")]));
%		false -> 
%			undefined
%		end
%	end.

%%
%load_path1(Path) ->
%	BeamDir = filename:join(Path, "ebin"),
%	%?TTY("Trying: ~p~n", [Path]),
%	case filelib:wildcard("*" ++ ?CONFIG_FILE_EXT, BeamDir) of
%	[AppFile|_] ->
%		%?TTY("AppFile: ~p~n", [{AppFile, BeamDir}]),
%		AppName = list_to_atom(filename:basename(AppFile, ?CONFIG_FILE_EXT)),
%		code:add_pathz(BeamDir),
%		Valid = validate(AppName),
%		#web_app{id=AppName, path=Path, valid=Valid};
%	_ -> 
%		undefined
%	end.
	
%% stub
%validate(_Name) -> ok.

%load_template(undefined, Path) -> 
%	{error, {undefined, Path}};
%load_template(Dir, Path) ->
%	File = filename:join([code:lib_dir(ewok), Dir, Path]),
%	case filelib:is_regular(File) of
%	true -> 
%		{ok, Bin} = file:read_file(File),
%		parse_template(Bin);
%	false -> 
%		{error, File}
%	end.

%% stub
%parse_template(_Bin) -> ok.

%%
%load_termfile(App) ->
%	case code:ensure_loaded(App) of
%	{'module', App} ->
%		Path = filename:dirname(code:which(App)),
%		File = filename:join(Path, atom_to_list(App) ++ ?CONFIG_FILE_EXT),
%		case filelib:is_regular(File) of
%		true ->
%			try
%				{ok, Terms} = file:consult(File),
%				{ok, File, Terms}
%			catch
			%% C{error,{103,erl_parse,["syntax error before: ","'{'"]}}
%			_:{badmatch, {error, {Line, _, Message}}} -> 
%				{error, {file, File}, {line, Line}, {reason, lists:flatten(Message)}};
%			E:R -> {E, R}
%			end;
%		false ->
%			{error, {nofile, File}}
%		end;
%	_ -> {error, {no_app_found, App}}
%	end.


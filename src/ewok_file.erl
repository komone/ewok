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

-include("ewok.hrl").
-include("ewok_system.hrl").
-include_lib("kernel/include/file.hrl").
-include_lib("stdlib/include/zip.hrl").

-export([open/2, close/1, filename/1]).
-export([path/1, parent/1, name/1, appname/1, extension/1]).
-export([load/1, save/2, eval/1, resource/2]).
-export([list/1, code_path/1]). 
-export([type/1, modified/1]).
-export([is_directory/1, is_file/1, is_regular/1, is_link/1]).
-export([file_info/1, find/2, find/3]).

-define(PATH_SEPARATOR, <<"[\\\\/]">>).
-define(BIN_DIR, <<"ebin">>).

-compile(export_all).
test() ->
	path(".").
	

%% NOTE: /.. -> /
path(Bin) when is_binary(Bin) ->
	path([Bin]);
path(List) when ?is_string(List) -> % "strings"
	path([list_to_binary(List)]);
path(List) ->	
	Filtered = filter_paths(List, [cwd()]),
	Parts = [ewok_text:split(X, ?PATH_SEPARATOR) || X <- Filtered],
%	?TTY({parts, lists:append(Parts)}),
	make_path(lists:append(Parts), []).

%% @private
%% convert "string" elements 
filter_paths([H|T], Acc) when ?is_string(H) ->
	filter_paths([list_to_binary(H)|T], Acc);
%% drop elements that precede an absolute path
filter_paths([H = <<$/, _/binary>>|T], _Acc) ->
	filter_paths(T, [H]);
filter_paths([Windoze = <<_, $:, _/binary>>|T], _Acc) ->
	filter_paths(T, [Windoze]);
%% add others
filter_paths([H|T], Acc) ->
	filter_paths(T, [H|Acc]);
filter_paths([], Acc) ->
	lists:reverse(Acc).

%% @private
% current
make_path([<<$.>>|T], Acc) ->
	make_path(T, Acc);
% parent
make_path([<<"..">>|T], [_, <<$/>> | T0]) ->
	make_path(T, T0);
% windoze
make_path([H = <<_, $:>>|T], []) ->
	make_path(T, [ewok_text:to_upper(H)]);
%
make_path([H|T], Acc) when is_binary(H) ->
	make_path(T, [H, <<$/>> | Acc]);
%
make_path([], Acc) ->
	list_to_binary(lists:reverse(Acc)).
	
%%
cwd() ->
	{ok, CD} = file:get_cwd(),
	list_to_binary(CD).

%% TODO: test
code_path(Path) ->
	case is_regular(Path) of
	true ->
		case extension(Path) of
		<<".ez">> ->
			archive_code_path(Path);
		_ -> 
			Parent = parent(Path),
			case name(Parent) of
			?BIN_DIR ->
				Parent;
			_ -> 
				undefined
			end
		end;
	_ ->
		case is_directory(Path) andalso name(Path) of
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
filename(Path) ->
	lists:last(ewok_text:split(Path, ?PATH_SEPARATOR)).
%%
name(Path) ->
	File = lists:last(ewok_text:split(Path, ?PATH_SEPARATOR)),
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
		[AppName|_Version] = ewok_text:split(Name, <<$->>),
		binary_to_atom(AppName, utf8);
	_ ->
		undefined
	end.

%%
extension(Path) ->
	File = lists:last(ewok_text:split(Path, ?PATH_SEPARATOR)),
	case lists:reverse(ewok_text:split(File, <<"(\\.)">>)) of
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
eval(File) ->
	Path = path(File),
	case is_regular(Path) of
	true ->
		try
			{ok, Term} = file:consult(binary_to_list(Path)),
			Term
		catch
		%% C{error,{103,erl_parse,["syntax error before: ","'{'"]}}
		_:{badmatch, {error, {Line, _, Message}}} -> 
			{error, [{file, Path}, {line, Line}, {reason, lists:flatten(Message)}]};
		Error:Reason -> 
			{Error, Reason}
		end;
	false ->
		{error, Path}
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
	
type(File) ->
	case file_info(File) of
	#file_info{type = Type} ->
		Type;
	_ ->
		undefined
	end.

%% Temp...?
modified(File) ->
	Info = file_info(File),
	Info#file_info.mtime.
	
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
	{ok, Info} -> 
		Info;
	_ -> 
		undefined
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

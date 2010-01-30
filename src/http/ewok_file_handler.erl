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

-module(ewok_file_handler).
-include("ewok.hrl").
-include("ewok_system.hrl").

-compile(export_all).

-behaviour(ewok_http_resource).
-name("Ewok Static File Handler").
-export([filter/1, resource_info/0]).
%%
-export(['GET'/2]).
%% NOTE: in general, it would be much better to serve static files
%% from the reverse proxy, however, if you insist...
%% NOTE: in production mode, static files will be cached in memory
%% NOTE: sending not_modified (at least to IE and chrome) causes
%% the socket connection to close... bug? or expected behavior? Firefox doesn't 
%% close out... update: Chrome developers have this as a registered bug in their browser.

%%
%% Resource Callbacks
%%
resource_info() ->
	{resource_info, [
		{description, "Serves static files from www_root"}
	]}.
	
%% This filter includes delegation to the ESP file interceptor
filter(Request) ->
	case ewok_file:extension(Request:path()) of
	?ESP_FILE_EXT -> 
		{delegate, esp_page_handler, []};
	_ -> 
		ok
	end.

%%
'GET'(Request, Session) ->
	case get_file(Request) of
	File = #ewok_file{} -> 
		LastModified = File#ewok_file.modified,
		Headers = [
			{content_type, File#ewok_file.mimetype},
			{content_length, File#ewok_file.size},
			{last_modified, LastModified}
		],
		case Request:header(if_modified_since) of
		LastModified ->
			{not_modified, Headers, []};
		_ ->
			Content = read_file(File),
			{ok, Headers, Content}
		end;
	undefined -> 
		ewok_web:errorpage(Request, Session, not_found);
	_ -> 
		internal_server_error
	end.
	
%% -> #ewok_file | undefined
get_file(Request) ->
	Realm = Request:realm(),
	Path = Request:path(),
	case ewok:config({ewok, runmode}) of
	development ->
		get_file(Realm, Path);
	production ->
		case ewok_cache:lookup(ewok_file, Path) of
		File = #ewok_file{} -> 
			File;
		undefined -> 
			cache_file(Realm, Path)
		end
	end.

%%
get_file(Realm, Path) ->
	AppDir = ewok_util:appdir(Realm),
	AppPath = ewok:config({Realm, http, www_root}, ?WWW_ROOT),
	FilePath = ewok_file:path([AppDir, AppPath, <<$., Path/binary>>]),
	File = 
		case ewok_file:is_directory(FilePath) of
		true ->
			IndexFile = ewok:config({Realm, http, index_file}, ?WWW_INDEX_FILE),
			ewok_file:path([FilePath, IndexFile]);
		false -> 
			FilePath
		end,
	case ewok_file:is_regular(File) of
	true ->
		ewok_file:resource(File, Path);
	false ->
		undefined
	end.
	
%%
cache_file(Realm, Path) ->
	MaxFileSize = ewok:config({ewok, http, cache, max_file_size}, infinity),
	case get_file(Realm, Path) of
	File = #ewok_file{} when File#ewok_file.size =< MaxFileSize ->
		{ok, Binary} = file:read_file(File#ewok_file.route),
		CachedFile = File#ewok_file{bin=Binary},
		ewok_cache:add(CachedFile),
		CachedFile;
	File = #ewok_file{} ->
		File;
	undefined -> 
		undefined
	end.
	
%%
read_file(#ewok_file{size=Size, bin=Bin}) 
		when is_binary(Bin) 
		andalso size(Bin) =:= Size ->
	Bin;
read_file(#ewok_file{file=File}) when is_binary(File) ->
	ewok_file:load(File).

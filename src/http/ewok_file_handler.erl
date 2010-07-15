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
resource_info() -> [
	{description, "Serves static files from www_root"}
].
	
%% This filter includes delegation to the ESP file interceptor
filter(#http_request{path = Path}) ->
	case ewok_file:extension(Path) of
	?ESP_FILE_EXT -> 
		{delegate, esp_page_handler, []};
	_ -> 
		ok
	end.

%%
'GET'(Request = #http_request{headers = Headers}, Session) ->
	case get_file(Request) of
	File = #ewok_file{modified = LastModified} -> 
		NewHeaders = [
			{content_type, File#ewok_file.mimetype},
			{content_length, File#ewok_file.size},
			{last_modified, LastModified}
		],
		% NOTE: cache hack follows - revisit this
		CacheControl = proplists:get_value(cache_control, Headers),
		IfModifiedSince = proplists:get_value(if_modified_since, Headers),
		case {CacheControl, IfModifiedSince} of
		{undefined, LastModified} ->
			{not_modified, NewHeaders, []};
		_ ->
			Content = read_file(File),
			{ok, NewHeaders, Content}
		end;
	undefined -> 
		ewok_web:errorpage(Request, Session, not_found);
	_ -> 
		internal_server_error
	end.
	
%% -> #ewok_file | undefined
get_file(#http_request{realm = Realm, path = Path}) ->
	case ewok_config:get_value({ewok, runmode}) of
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
	AppPath = ewok_file:path([ewok_config:get_value({Realm, http, doc_root}, ?WWW_ROOT)]),
	FilePath = ewok_file:path([AppDir, AppPath, <<$., Path/binary>>]),
	Size = size(AppPath),
	case FilePath of
	<<AppPath:Size/binary, _/binary>> ->	
		% ?TTY({found, FilePath, ewok_file:type(FilePath)}),	
		case ewok_file:type(FilePath) of
		directory ->
			get_index_file(Realm, Path, FilePath);
		regular -> 
			ewok_file:resource(FilePath, Path);
		_ -> 
			undefined
		end;
	_ ->
		undefined
	end.
	
get_index_file(Realm, Path, FilePath) ->
	IndexFileName = ewok_config:get_value({Realm, http, index_file}, ?WWW_INDEX_FILE),
	IndexPath = ewok:path([FilePath, IndexFileName]),
	case ewok_file:type(IndexPath) of
	regular ->
		ewok_file:resource(IndexPath, Path);
	_ ->
		undefined
	end.
	
%%
cache_file(Realm, Path) ->
	MaxFileSize = ewok_config:get_value({ewok, http, cache, max_file_size}, infinity),
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

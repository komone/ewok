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

-module(esp_page_handler).

-include("ewok.hrl").
-include("ewok_system.hrl").
%-include("esp.hrl").

-behavior(ewok_http_resource).
-export([filter/1, resource_info/0]).

-export(['GET'/2]).

%%
-record(esp_cache, {route, path, mimetype, modified, bin=undefined}).

%%
%% Resource Callbacks
%%
resource_info() ->
	{resource_info, [
		{name, "Ewok ESP File Handler"},
		{version, {1,0,0}},
		{description, "Serves esp files from www_root"}
	]}.
	
%%
filter(Request) ->
	case ewok_file:extension(Request:path()) of
	?ESP_FILE_EXT -> ok;
	_ -> % is this the right thing to do??
		bad_request 
	end.
	
%%
'GET'(Request, Session) ->
	case get_file(Request:path()) of 
	#esp_cache{} = File ->
		Spec = esp:parse_template(File#esp_cache.bin),
		%?TTY("SPEC: ~p~n", [Spec]),
		case esp:render_page(Spec, ?MODULE, Request, Session) of
		{ok, Binary} ->
			Headers = [
				{content_type, File#esp_cache.mimetype}, 
				{content_length, size(Binary)},
				{last_modified, ewok_http:date()},
				{cache_control, <<"no-cache">>}
			],
			{ok, Headers, Binary};
		_ ->
			internal_server_error
		end;
	undefined ->
		not_found
	end.

%%
%% Internal
%%

%% RelativePath::string() -> #esp_cache{} | undefined
get_file(Path) ->
	case ewok_cache:lookup(esp_cache, Path) of
	undefined ->
		File = read_file(Path),
		case File of
		#esp_cache{} -> %% maybe cache...
			case ewok:config({ewok, runmode}) of
			production -> 
				ewok_cache:add(File);
			_ -> 
				ok
			end;
		undefined -> 
			undefined
		end,
		File;
	File -> 
		File
	end.
%
read_file(<<$/, Path/binary>>) ->
	Dir = ewok:config({ewok, http, doc_root}, ?WWW_ROOT),
	File = ewok_file:path([Dir, Path]), 
%	?TTY({?MODULE, Dir, Path, File}),
	case ewok_file:is_regular(File) of
	true -> 
		Bin = ewok_file:load(File),
		%% TODO: NOTE: owing to index files two copies of indexes may be stored
		%% under different routes e.g. both /stuff and /stuff/index.html
		%% -- not sure what is the best solution to avoid this just yet...
		#esp_cache{
			route = Path,
			path = File,
			mimetype = ewok_http:mimetype(ewok_file:extension(File)),
			modified = ewok_http:date(ewok_file:modified(File)),
			bin=Bin
		};
	false -> 
		undefined
	end.

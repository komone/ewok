%%
-module(esp_page_handler).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl").
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
	case filename:extension(Request:path()) of
	?ESP_FILE_EXT -> ok;
	_ -> % is this the right thing to do??
		bad_request 
	end.
	
%%
'GET'(Request, Session) ->
	case get_file(Request:path()) of 
	File when is_record(File, esp_cache) ->
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
		_ when is_record(File, esp_cache) -> %% maybe cache...
			case ewok:config({ewok, runmode}) of
			production -> ewok_cache:add(File);
			_ -> ok
			end;
		undefined -> 
			undefined
		end,
		File;
	File -> 
		File
	end.
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

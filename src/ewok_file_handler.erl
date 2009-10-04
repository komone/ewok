%%
-module(ewok_file_handler).
-vsn("1.0").
-author('steve@simulacity.com').

-include("../include/ewok.hrl").

%% NOTE: in general, it would be much better to serve static files
%% from the reverse proxy, however, if you insist...
%% NOTE: in production mode, static files will be cached in memory
%% NOTE: sending not_modified (at least to IE and chrome) causes
%% the socket connection to close... bug? or expected behavior? Firefox doesn't 
%% close out... update: Chrome developers have this as a registered bug in their browser.

-compile(export_all).

-behavior(ewok_http_resource).
-export([filter/1, resource_info/0]).

%%
-export(['GET'/2]).
%%
-record(file_cache, {route, path, mimetype, modified, size=0, bin=undefined}).

%%
%% Resource Callbacks
%%
resource_info() ->
	{resource_info, [
		{name, "Ewok Static File Handler"},
		{version, "1.0"},
		{description, "Serves static files from www_root"}
	]}.
	
%% This filter includes delegation to the ESP file interceptor
filter(Request) ->
	case filename:extension(Request:path()) of
	?ESP_FILE_EXT -> {delegate, esp_page_handler, []};
	_ ->
		case Request:method() of
		'HEAD' -> ok;
		'GET' -> ok;
		_ -> method_not_allowed
		end
	end.

%%
'GET'(Request, Session) ->
	case get_file(Request) of 
	File when is_record(File, file_cache) ->
		Headers = [
			{content_type, File#file_cache.mimetype}, 
			{content_length, File#file_cache.size}, %% NOTE: is this value always valid...?
			{last_modified, File#file_cache.modified}
		],		
		LastModified = File#file_cache.modified,
		case Request:header(if_modified_since) of
		LastModified ->
			{not_modified, Headers, []}; %% BUG?: why does this response cause the connection to close?
		_ ->
			case File#file_cache.bin of
			undefined ->
				{ok, Headers, {file, File#file_cache.path}};
			_ ->
				{ok, Headers, File#file_cache.bin}
			end
		end;
	undefined ->
		ewok_web:errorpage(Request, Session, not_found)
	end.

%%
%% Internal
%%
get_file(Request) ->
	Path = Request:path(),
	% If the server is in production mode static files are usually cached in RAM...
	case ewok_cache:lookup(file, Path) of
	undefined -> 
		AppPath = ewok:config({Request:realm(), http, app_path}, "/"),
		FilePath = 
		%% Paths MUST be strings to allow filename joins in get_file/2 to work!
			case re:split(Path, AppPath, [{return, list}]) of
			[[], []] when AppPath =/= "/" -> "/";
			[[], Value] when AppPath =/= "/" -> Value;
			_ -> Path 
			end,
		case get_file(Request, FilePath) of
		undefined -> undefined;
		NewFile -> %% maybe cache...
			Limit = ewok:config({ewok, http, cache, max_file_size}, -1),
			case ewok:config({ewok, runmode}) of
			production when NewFile#file_cache.size =< Limit ->
				{ok, Binary} = file:read_file(NewFile#file_cache.path),
				CacheableFile = NewFile#file_cache{bin=Binary},
				ewok_cache:add(CacheableFile),
				CacheableFile;
			_ -> 
				NewFile
			end
		end;
	CachedFile -> 
		?TTY("From cache ~p (~p)~n", [Path, ewok:config({ewok, runmode})]),
		CachedFile
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

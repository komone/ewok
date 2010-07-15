-module(ewok_http_router).

-include("ewok.hrl").
-include("ewok_system.hrl").

-export([lookup/1, uri_to_path/1]).

%%
lookup(Path) ->
	case ewok_db:lookup(ewok_route, Path) of
	Route = #ewok_route{} -> 
		Route;
	undefined -> 
		PathComponents = ewok_text:split(Path, <<"/">>),
		lookup_wildcard_route(lists:reverse(PathComponents));
	Error -> 
		Error
	end.
%% note that since ewok_file_handler is usually the default -- ALL file urls 
%% will be processed here -> could be a problem...
%% Turning this into a gb_tree may possibly improve wildcard lookup/support for REST?
%% Well, first, let's see if it shows up in profiling under load.
lookup_wildcard_route(Parts = [_|T]) ->
	case ewok_db:lookup(ewok_route, wildcard_path(Parts)) of
	undefined -> 
		lookup_wildcard_route(T);
	Route = #ewok_route{} -> 
		Route;
	Error -> 
		Error
	end;
lookup_wildcard_route([]) ->
	case ewok_db:lookup(ewok_route, default) of
	undefined -> 
		{error, no_handler};
	Route = #ewok_route{} -> 
		Route;
	Error -> 
		Error
	end.
%
wildcard_path(Parts) ->
	list_to_binary([[<<$/>>, X] || X <- lists:reverse([<<$*>>|Parts])]).

%% NOTE: these are types returned from inets http, may not be needed
uri_to_path({abs_path, Path}) -> 
	Path;
%% Detect this case to find out when/if it happens...
uri_to_path(URI = {absoluteURI, _Protocol, _Host, _Port, Path}) ->
	ewok_log:warn([{request_uri, URI}]),
	Path;
%% From http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2
%% e.g. OPTIONS * HTTP/1.1 (used for proxies etc)
uri_to_path(Path = '*') -> 
	ewok_log:warn([{request_uri, Path}]),
	Path;
%% This should probably never happen... right?
uri_to_path(URI) -> 
	ewok_log:error([{request_uri, URI}]),
	URI.


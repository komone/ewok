%%
-module(ewok_http_srv).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl").

-behavior(ewok_service).
-export([start_link/0, stop/0, service_info/0]).

%% API
-export([service/2, lookup_route/1]).

%% temp
-export([cookie_compare/2]).

%%
service_info() -> [
	{name, "Ewok HTTP Service"},
	{version, {1,0,0}},
	{comment, ""},
	{depends, [ewok_cache_srv, ewok_session_srv]}
].

%%
start_link() ->
	ewok:config(),
	Port = ewok_config:get({ewok, http, port}, 8000),
	Timeout = ewok_config:get({ewok, http, request_timeout}, 30) * 1000,
	Handler = fun(X) -> ?MODULE:service(X, Timeout) end,
	ewok_log:log(default, service, {?MODULE, service_info()}),
	ewok_log:log(default, configuration, {?MODULE, [{port, Port}, {request_timeout, Timeout}]}),

	{ok, Pid} = ewok_tcp_srv:start_link(?MODULE, [{port, Port}, {handler, Handler}]),

	ewok_log:add_log(access),
	ewok_log:add_log(auth),
	{ok, Pid}.

%%	
stop() -> 
	ewok_tcp_srv:stop(?MODULE),
	ewok_log:remove_log(access).

%% Callback from ewok_tcp_srv, handing over a client connection
service(Socket, Timeout) ->
    ok = inet:setopts(Socket, [{packet, http_bin}]),
	case gen_tcp:recv(Socket, 0, Timeout) of
	{ok, {http_request, Method, URI, Version}} ->
		RequestLine = {Method, uri_to_path(URI), Version},
%		ewok_log:info([{request, RequestLine}]),
		get_headers(Socket, RequestLine, [], Timeout);
	{error, Reason} ->
		ewok_log:warn([{connection, Reason}]),
		gen_tcp:close(Socket),
		exit(normal)
    end.

%%
get_headers(Socket, RequestLine = {Method, Path, Version}, Headers, Timeout) ->
    case gen_tcp:recv(Socket, 0, Timeout) of
	{ok, {http_header, _, Name, _, Value}} ->
		HeaderName =
			case Name of
			_ when is_atom(Name) -> atom_to_binary(Name, utf8); % would latin1 be "safer"?
			_ when is_binary(Name) -> Name
			end,
		get_headers(Socket, RequestLine, [{HeaderName, Value} | Headers], Timeout);
	{ok, http_eoh} ->
		inet:setopts(Socket, [{packet, raw}]), 
		Request = ewok_request_obj:new(Socket, Timeout, Method, Path, Version, Headers),
		get_session(Request);
	{error, Reason} ->
		?TTY("Socket Error... ~p~n", [Reason]), 
		gen_tcp:close(Socket),
		exit(normal)
    end.
	
%% WHO
get_session(Request) ->
	Session = ewok_session_srv:get_session(Request:cookie(), Request:remote_ip()),
	case Request:version() of
	{1, 1} -> 
		get_route(Request, Session);
	_ -> 
		?TTY("~p~n", [Request]),
		do_response(Request, Session, http_version_not_supported)
	end.

%% WHERE
%% NOTE: there's a good case to invert the argument ordering as it progresses from here
get_route(Request, Session) ->
	case lookup_route(Request:path()) of
	Route when is_record(Route, route) ->
		Request:set_realm(Route#route.realm),
		get_access(Request, Session, Route);
	_ ->
		do_response(Request, Session, internal_server_error)
	end.

% HOW
get_access(Request, Session, #route{handler=Module, realm=Realm, roles=Roles}) ->	
	case validate_access(Session:user(), Realm, Roles) of
	ok -> 
		handle_request(Request, Session, Module);
	not_authorized ->
		case ewok_config:get({Realm, http, login}) of
		undefined -> 
			%?TTY("Access denied~n", []),
			do_response(Request, Session, unauthorized);
		LoginPath ->
			% it may be cleaner to use a module (other than proplists) that implement set/replacekey
			Here = ewok_http:absolute_uri(Request:path()),
			Session:save({redirect, Here}),
			Location = ewok_http:absolute_uri(LoginPath),
			%% NOTE: found = 302 - but perhaps temporary_redirect 307 would be better?
			do_response(Request, Session, {found, [{location, Location}], []})
		end
	end.

%% WHAT
%% NOTE: Everything else is just boilerplace compared to this...
handle_request(Request, Session, Module) ->
	ewok_log:info([{call, {Request:path(), Module, Session:data()}}]),
	Response = 
		case code:ensure_loaded(Module) of
		{'module', Module} ->
			case Module:filter(Request) of
			{delegate, Handler, Options} when is_atom(Handler), is_list(Options) -> 
				ewok_log:info([{delegated, {Handler, Options}}]),
				Session:save({delegated, Options}),
				%% TODO: how to implement delegation Options for a handler without making
				%% the handler's client code onerous?
				handle_request(Request, Session, Handler);
			ok ->
				Method = Request:method(),
				%% TODO - we may wish to trap 'OPTIONS' here
				case erlang:function_exported(Module, Method, 2) of
				true -> Module:Method(Request, Session);
				false ->
					%% IMPL: if possible, autogenerate a response for 'HEAD'
					case Method =:= 'HEAD' 
						andalso erlang:function_exported(Module, 'GET', 2) of
					true -> 
						case Module:'GET'(Request, Session) of
						{ok, Headers, _} -> {ok, Headers, []};
						Result -> Result
						end;
					false ->
						method_not_allowed
					end
				end;
			_ ->
				precondition_failed
			end;
		Error ->
			ewok_log:error(Error),
			internal_server_error
		end,
	do_response(Request, Session, Response).

%%
do_response(Request, Session, Status) when is_atom(Status) ->
	Code = ewok_http:status_code(Status),
	do_response(Request, Session, {Code, [], [ewok_http:status_message(Code)]});
%
do_response(Request, Session, Status) when is_integer(Status) ->
	do_response(Request, Session, {Status, [], [ewok_http:status_message(Status)]});
%
do_response(Request, Session, {Status, Headers}) ->
	do_response(Request, Session, {Status, Headers, []});
% by this time, we should have a full response
do_response(Request, Session, {Status, Headers, Content}) ->	
	Code = ewok_http:status_code(Status),
	%
	ewok_session_srv:update_session(Session),
	%
	NewHeaders =
		case cookie_compare(Request:cookie(), Session:cookie()) of
		false -> [{set_cookie, Session:cookie()}|Headers];
		true -> Headers
		end,
	Response = {response, Code, NewHeaders, Content, false},
	{ok, _HttpResponse, BytesSent} = ewok_response:reply(Request, Response),
	%
	{Tag, Message} = format_access_log(Request, Session, Code, BytesSent),
	ok = ewok_log:log(access, Tag, Message),
	%
	cleanup(Request, Session).

%%
cleanup(Request, Session) ->
	% ?TTY("REQ:~n~p~n", [Request]),
	Socket = Request:socket(),
	case Request:should_close() of
	true ->
		ewok_session_srv:close_session(Session),
		receive
			Message = {session_end, _Type, _Key} -> ?TTY("~p~n", [Message])
		after 1000 -> 
			ok
		end,
		?TTY("Closed ~p~n", [Socket]),
		gen_tcp:close(Socket),
		exit(normal);
	false ->
		%?TTY("Connection ~p~n", [Request:header(connection)]),
		%% NOTE: IE and Chrome (not Firefox) will close out if 304 not_modified returned
		%% even though they ask for keep-alive... weird! Chrome lists this as a bug in 
		%% the browser so it will be fixed, which again leaves IE as the toy.
		Timeout = Request:timeout(), 
		Session:reset(),
		Request:reset(),
		?MODULE:service(Socket, Timeout)
	end.

%%
%% INTERNAL
%%

%% There is a more elegant way to do this using binary matching... I'm certain of it.
cookie_compare(RequestCookie, SessionCookie) -> 
	case re:split(SessionCookie, <<";">>) of
	[RequestCookie|_] -> true;
	_ -> false
	end.

%%
uri_to_path({abs_path, Path}) -> 
	Path;
%% detect this case to find out when/if it happens...
uri_to_path(URI = {absoluteURI, _Protocol, _Host, _Port, Path}) ->
	ewok_log:warn([{request_uri, URI}]),
	Path;
%% From http://www.w3.org/Protocols/rfc2616/rfc2616-sec5.html#sec5.1.2
%% e.g. OPTIONS * HTTP/1.1 (used for proxies etc)
uri_to_path(Path = '*') -> 
	ewok_log:warn([{request_uri, Path}]),
	Path;
%% Should never happen...
uri_to_path(URI) -> 
	ewok_log:error([{request_uri, URI}]),
	URI.

%%
lookup_route(Path) ->
	case ewok_cache:lookup(route, Path) of
	Route when is_record(Route, route) -> Route;
	undefined -> 
		Parts = 
			case re:split(Path, "/", [{return, list}, trim]) of
			[[]|List] -> List; % ugh
			[] -> []
			end,
		lookup_wildcard_route(lists:reverse(Parts));
	Error -> Error
	end.
%% note that since ewok_file_handler is usually the default -- ALL file urls 
%% will be processed here -> could be a problem...
%% Turning this into a gb_tree may possibly improve wildcard lookup/support for REST?
lookup_wildcard_route(Parts = [_|T]) ->
	Path = lists:append("/", filename:join(lists:reverse(["*"|Parts]))),
	case ewok_cache:lookup(route, Path) of
	Route when is_record(Route, route) -> Route;
	undefined -> lookup_wildcard_route(T);
	Error -> Error
	end;
lookup_wildcard_route([]) ->
	case ewok_cache:lookup(route, default) of
	Route when is_record(Route, route) -> Route;
	undefined -> {error, no_handler};
	Error -> Error
	end.

%%
validate_access(_, _, any) -> ok;
validate_access(undefined, _, _) -> not_authorized;
validate_access(#user{roles = UserRoles}, Realm, ResourceRoles) ->
	Auth = lists:map(fun (R = {_, _}) -> R; (R) -> {Realm, R} end, ResourceRoles),
	case [X || X <- UserRoles, Y <- Auth, X =:= Y] of
	[] -> not_authorized;
	_ -> ok
	end.

%% TODO: Move this later on
format_access_log(Request, Session, StatusCode, BytesSent) ->
	UserId = 
		case Session:user() of
		User when is_record(User, user) -> 
			list_to_binary(io_lib:format("~p", [User#user.name]));
		_ -> <<"{-,-}">>
		end,
		
	{Major, Minor} = Request:version(),
	Version = list_to_binary(io_lib:format("HTTP/~w.~w", [Major, Minor])),
	Status = list_to_binary(integer_to_list(StatusCode)),
	RequestLine = list_to_binary([
		Request:remote_ip(), <<" ">>, 
		UserId, <<" ">>,
		<<$">>, atom_to_binary(Request:method(), utf8), <<" ">>,
		Request:url(), <<" ">>, Version,
		<<$">>, <<" ">>,
		list_to_binary(integer_to_list(BytesSent)), <<" ">>,
		ewok_http:browser_detect(Request:header(<<"User-Agent">>))
	]),
	{Status, RequestLine}.

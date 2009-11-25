%%
%%
-module(ewok_http_srv).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl").

-behavior(ewok_service).
-export([start_link/0, stop/0, service_info/0]).

%% API
-export([service/3]).
% There will probably be a use for this API export, but
% if no genuine use for it is actually found, then hide it.
-export([lookup_route/1]). 
-export([connections/0]).

-define(DEPENDS, [ewok_cache_srv, ewok_session_srv]).

%%
service_info() -> [
	{name, "Ewok HTTP Service"},
	{version, {1,0,0}},
	{comment, ""},
	{depends, ?DEPENDS}
].

%
connections() ->
	ewok_socket_srv:connections(?MODULE).
	
%%
start_link() ->
	try begin
		ewok_log:log(default, service, {?MODULE, service_info()}),		
		ewok_util:check_dependencies(?DEPENDS),
		
		Transport = gen_tcp, %% TEMP
		SocketOpts = ewok_socket_srv:configure(Transport, "ewok.http"),
		Port = ewok:config({ewok, http, port}, 8080),
		MaxConnections = ewok:config({ewok, http, tcp, max_connections}, infinity),
		
		Timeout = ewok:config({ewok, http, request_timeout}, 30) * 1000,
		MaxHeaders = ewok:config({ewok, http, header_limit}, 100),
		Handler = fun(X) -> ?MODULE:service(X, Timeout, MaxHeaders) end,
		
		Configuration = [
			{name, ?MODULE},
			{transport, Transport}, %% defines use of gen_tcp or ssl module
			{port, Port},
			{protocol, http},
			{max_connections, MaxConnections},
			{socket_opts, SocketOpts},
			{handler, Handler}
		],
		%?TTY("CONFIG:~n~p~n", [TCPConfiguration]),
		
		ewok_log:log(default, configuration, {?MODULE, Configuration}),
		
		%% Starts a TCP Server for HTTP
		{ok, Pid} = ewok_socket_srv:start_link(?MODULE, Configuration),
		
		ewok_log:add_log(access),
		ewok_log:add_log(auth),
		{ok, Pid}
	end catch
	Error:Reason -> {Error, Reason}
	end.

%%	
stop() -> 
	ewok_tcp_srv:stop(?MODULE),
	ewok_log:remove_log(auth),
	ewok_log:remove_log(access).

%% Callback from ewok_tcp_srv, handing over a client connection
service({Transport, Socket}, Timeout, MaxHeaders) ->
	ok = 
		case Transport of 
		gen_tcp -> inet:setopts(Socket, [{packet, http_bin}]);
		ssl -> ssl:setopts(Socket, [{packet, http_bin}])
		end,
	case Transport:recv(Socket, 0, Timeout) of
	{ok, {http_request, Method, URI, Version}} ->
		RequestLine = {Method, uri_to_path(URI), Version},
%		ewok_log:debug([{request, RequestLine}]),
%		ok = inet:setopts(Socket, [{packet, httph_bin}]),
		ok = 
			case Transport of 
			gen_tcp -> inet:setopts(Socket, [{packet, httph_bin}]);
			ssl -> ssl:setopts(Socket, [{packet, httph_bin}])
			end,
		get_headers({Transport, Socket}, RequestLine, [], Timeout, 0, MaxHeaders);
	%% TODO: is there any reason that would mean that we can/should continue?
	{error, Reason} ->
		ewok_log:warn([{connection, Reason}]),
		Transport:close(Socket),
		exit(normal)
    end.

%%
get_headers({Transport, Socket}, RequestLine, Headers, _Timeout, MaxHeaders, MaxHeaders) ->
	ProxyHeader = proplists:get_value(<<"X-Forwarded-For">>, Headers),
	ewok_log:error([
		{message, "Too many headers"}, 
		{request_line, RequestLine}, 
		{remote_ip, ewok_http:get_remote_ip(Socket, ProxyHeader)}
	]),
	%% NOTE: Consider sending the response 'bad_request' instead of just closing out?
	Transport:close(Socket),
	exit(normal);
%%
get_headers({Transport, Socket}, RequestLine = {Method, Path, Version}, Headers, Timeout, Count, MaxHeaders) ->
	case Transport:recv(Socket, 0, Timeout) of
	{ok, {http_header, _Integer, Name, _Reserved, Value}} ->
		HeaderName =
			case is_atom(Name) of
			true -> atom_to_binary(Name, utf8); % would latin1 be "safer"?
			false -> Name % must be a binary
			end,
		NewCount = 
			case is_integer(Count) of
			true -> Count + 1;
			false -> Count %% i.e. 'infinity'
			end,
		get_headers({Transport, Socket}, RequestLine, [{HeaderName, Value} | Headers], Timeout, NewCount, MaxHeaders);
	{ok, http_eoh} ->
%		ok = inet:setopts(Socket, [{packet, raw}]), 
		ok = 
			case Transport of 
			gen_tcp -> inet:setopts(Socket, [{packet, raw}]);
			ssl -> ssl:setopts(Socket, [{packet, raw}])
			end,
		Request = ewok_request_obj:new(Transport, Socket, Timeout, Method, Path, Version, Headers, MaxHeaders),
		get_session(Request);
%% IMPL: If the client screws up should we be strict (as currently), or lenient (as below)...
%%	{error, {http_error, <<$\r, $\n">>}} ->
%%		get_headers(Socket, RequestLine, Timeout, Count, MaxHeaders);
%%	{error, {http_error, <<$\n>>}} ->
%%		get_headers(Socket, RequestLine, Timeout, Count, MaxHeaders);
	%% TODO: is there any other reason that would mean that we can/should continue?
	{error, {http_error, Reason}} ->
		ewok_log:error([{http_error, Reason}]), 
		Transport:close(Socket), 
		exit(normal);
	{Error, Reason} ->
		ewok_log:error([{socket_error, {Error, Reason}}]), 
		Transport:close(Socket),
		exit(normal)
    end.
	
%% WHO
get_session(Request) ->
	Session = ewok_session:get_session(Request:cookie(), Request:remote_ip()),
%	?TTY("~p~n", [Request]),
	case Request:version() of
	{1, 1} -> 
		get_route(Request, Session);
	{1, 0} -> 
		get_route(Request, Session);
	Version -> 
		ewok_log:warn([
			{http_protocol, Version()}, 
			{user_agent, Request:header(user_agent)}
		]),
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
		do_response(Request, Session, internal_server_error) %% TODO: shouldn't this be 404?
	end.

% HOW
get_access(Request, Session, #route{handler=Module, realm=Realm, roles=Roles}) ->	
%	?TTY("REALM: ~p~n", [Request:realm()]),
	case validate_access(Session:user(), Realm, Roles) of
	ok -> 
		handle_request(Request, Session, Module);
	not_authorized ->
		case ewok:config({Realm, http, login}) of
		undefined -> 
			ewok_log:warn([{auth_login_undefined, Realm}]),
			do_response(Request, Session, unauthorized);
		LoginPath ->
			% it may be cleaner to use a module (other than proplists) that implements set/replacekey
			Here =
				case Request:header(referer) of
				undefined -> 
					ewok_http:absolute_uri(Request:path());
				Value -> Value
				end,
			Session:save({redirect, Here}),
			%Location = ewok_http:absolute_uri(LoginPath),
			%% NOTE: traditionally, this is 'found 302' - but 'temporary_redirect 307' may be better
			do_response(Request, Session, {found, [{location, LoginPath}], []})
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
	ewok_session:update(Session),
	% only set-cookie if cookie not set...
	NewHeaders =
		case cookie_compare(Request:cookie(), Session:cookie()) of
		false -> [{set_cookie, Session:cookie()}|Headers];
		true -> Headers
		end,
	Response = {response, Code, NewHeaders, Content, false},
	
	%% and finally...
	{ok, _HttpResponse, BytesSent} = ewok_response:reply(Request, Response),
	%%
	{Tag, Message} = format_access_log(Request, Session, Code, BytesSent),
	ok = ewok_log:log(access, Tag, Message),
	%%
	cleanup(Request, Session).

%%
cleanup(Request, Session) ->
	%?TTY("~nPD STATE~n~p~n", [lists:sort(get())]),
	{Transport, Socket} = Request:socket(),
	case Request:should_close() of
	true ->
		%ewok_session:close(Session),
		%receive
		%Message = {session_end, _Type, _Key} -> 
		%	?TTY("~p~n", [Message]),
		%	ok
		%after 1000 ->
		%	ok
		%end,
		?TTY("Closed ~p~n", [Socket]),
		Transport:close(Socket),
		exit(normal);
	false ->
		%?TTY("Connection ~p~n", [Request:header(connection)]),
		%% NOTE: IE and Chrome (not Firefox) will close out when 304 not_modified is 
		%% returned even though they ask for keep-alive... weird! Chrome lists this 
		%% as a fixed bug in their browser, which again leaves IE as the toy.
		Timeout = Request:timeout(), 
		MaxHeaders = Request:max_headers(),
		Session:reset(), %% ?? not sure we should do this...
		Request:reset(),
		?MODULE:service({Transport, Socket}, Timeout, MaxHeaders)
	end.

%%
%% INTERNAL
%%

%% TODO: This is a TOTAL HACK
%% There is a more elegant way to do this using binary matching... I'm certain of it.
cookie_compare([], _SessionCookie) ->
	false;
cookie_compare([{Tag, RequestCookie}], SessionCookie) -> 
%	?TTY("Cookie Compare ~p ~p~n", [RequestCookie, SessionCookie]),
	case re:split(SessionCookie, "[=;]") of
	[Tag, RequestCookie|_] -> true;
	_ -> false
	end.

%%
uri_to_path({abs_path, Path}) -> 
	Path;
%% Detect this case to find out when/if it happens...
%% IMPL: Ewok is not ever intended to support HTTPS. 
%% so consider setting the Protocol match as 'http'
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
%% Well, first, let's see if it shows up in profiling under load.
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

%% TODO: move to ewok_identity/ewok_auth... ** i.e. make pluggable
validate_access(_, _, any) -> ok;
validate_access(undefined, _, _) -> not_authorized;
validate_access(#user{roles = UserRoles}, Realm, ResourceRoles) ->
	Auth = lists:map(fun (R = {_, _}) -> R; (R) -> {Realm, R} end, ResourceRoles),
	case [X || X <- UserRoles, Y <- Auth, X =:= Y] of
	[] -> not_authorized;
	_ -> ok
	end.

%% TODO: Move this later on. To...?
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
	UserAgent =
		case Request:header(<<"User-Agent">>) of
		undefined -> <<"(No Header: User-Agent)">>;
		Value -> Value
		end,

	RequestLine = list_to_binary([
		Request:remote_ip(), <<" ">>, 
		UserId, <<" ">>,
		<<$">>, atom_to_binary(Request:method(), utf8), <<" ">>,
		Request:url(), <<" ">>, Version,
		<<$">>, <<" ">>,
		list_to_binary(integer_to_list(BytesSent)), <<" ">>,
		ewok_http:browser_detect(UserAgent)
	]),
	{Status, RequestLine}.

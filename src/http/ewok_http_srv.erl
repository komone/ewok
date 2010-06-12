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

-module(ewok_http_srv).
-name("Ewok HTTP Service").
-depends([ewok_db, ewok_session_srv]).

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_service).
-export([start_link/1, start_link/2, stop/0]).
-export([service/3]).

-define(DEFAULT_HTTP_PORT, 8080).

%%
start_link([]) ->
	start_link(ewok, ?DEFAULT_HTTP_PORT).
%
start_link(ServerId, Port) when is_atom(ServerId), is_integer(Port) ->
%	ewok_util:check_dependencies(?DEPENDS)
	try begin
		Transport = ewok_config:get_value({ServerId, transport}, gen_tcp),
		SocketOpts = ewok_socket:configure(Transport, {ServerId, http}),
		
		MaxConnections = ewok_config:get_value({ServerId, tcp, max_connections}, infinity),
		
		Timeout = ewok_config:get_value({ServerId, request_timeout}, 30) * 1000,
		MaxHeaders = ewok_config:get_value({ServerId, header_limit}, 100),
		
		Handler = fun(X) -> ?MODULE:service(X, Timeout, MaxHeaders) end,
		
		Configuration = [
			{transport, Transport}, %% defines use of gen_tcp or ssl module
			{port, Port},
			{protocol, http},
			{max_connections, MaxConnections},
			{socket_opts, SocketOpts},
			{handler, Handler}
		],
		
%		?TTY({config, {ServerId, Configuration}}),
		ewok_log:message(ServerId, {configuration, Configuration}),
		
		%% Starts a TCP Server for HTTP
		{ok, Pid} = ewok_socket_srv:start_link(ServerId, Configuration),
		
		ewok_log:add_log(access),
		ewok_log:add_log(auth),
		ewok_log:add_log(debug),
		{ok, Pid}
	end catch
	Error:Reason -> {Error, Reason}
	end.
%%	
stop() ->
	stop(?MODULE).
%%	
stop(ServerId) -> 
	case is_pid(whereis(ServerId)) of
	true ->
		ewok_socket_srv:stop(ServerId),
		ewok_log:remove_log(debug),
		ewok_log:remove_log(auth),
		ewok_log:remove_log(access);
	false ->
		{error, {not_started, ServerId}}
	end.

%% Callback from ewok_tcp_srv, handing over a client connection
service(Socket, Timeout, MaxHeaders) ->
	ok = ewok_socket:setopts(Socket, [{packet, http_bin}]),
	case ewok_socket:recv(Socket, 0, Timeout) of
	{ok, {http_request, Method, URI, Version}} ->
		RequestLine = {Method, uri_to_path(URI), Version},
		ok = ewok_socket:setopts(Socket, [{packet, httph_bin}]),
		get_headers(Socket, RequestLine, [], Timeout, 0, MaxHeaders);
	%% TODO: is there any reason that would mean that we can/should continue?
	{error, Reason} ->
		ewok_log:warn([{connection, Reason}]),
		ewok_socket:close(Socket),
		exit(normal)
    end.

%%
get_headers(Socket, RequestLine = {Method, Path, Version}, Headers, Timeout, HeadCount, MaxHeaders)  
		when HeadCount =< MaxHeaders -> %% NOTE: if MaxHeaders = 'infinity' then HeadCount =< MaxHeaders will always be true		
	case ewok_socket:recv(Socket, 0, Timeout) of
	{ok, {http_header, _Integer, Name, _Reserved, Value}} ->
		HeaderName =
			case is_atom(Name) of
			true -> 
				atom_to_binary(Name, utf8); % would latin1 be "safer"?
			false -> 
				Name % must be a binary
			end,
		get_headers(Socket, RequestLine, [{HeaderName, Value} | Headers], Timeout, HeadCount + 1, MaxHeaders);
	{ok, http_eoh} ->
		ok = ewok_socket:setopts(Socket, [{packet, raw}]),
		
		?TTY({request, Method, Path}),

		Request = ewok_request_obj:new(Socket, Timeout, Method, Path, Version, Headers, MaxHeaders),
		get_session(Request);
%% IMPL: If the client screws up should we be strict (as currently), or lenient (as below)...
%%	{error, {http_error, <<$\r, $\n">>}} ->
%%		get_headers(Socket, RequestLine, Timeout, Count, MaxHeaders);
%%	{error, {http_error, <<$\n>>}} ->
%%		get_headers(Socket, RequestLine, Timeout, Count, MaxHeaders);
	%% TODO: is there any other reason that would mean that we can/should continue?
	{error, {http_error, Reason}} ->
		ewok_log:error([{http_error, Reason}]), 
		ewok_socket:close(Socket), 
		exit(normal);
	{Error, Reason} ->
		ewok_log:error([{socket_error, {Error, Reason}}]), 
		ewok_socket:close(Socket),
		exit(normal)
    end;
%%
get_headers(Socket, RequestLine, Headers, _, _, _) ->
	ProxyHeader = proplists:get_value(<<"X-Forwarded-For">>, Headers),
	ewok_log:error([
		{message, "Too many headers"}, 
		{request_line, RequestLine}, 
		{remote_ip, ewok_http:get_remote_ip(Socket, ProxyHeader)}
	]),
	%% NOTE: Consider sending the response 'bad_request' instead of just closing out?
	ewok_socket:close(Socket),
	exit(normal).
	
%% WHO
get_session(Request) ->
	Session = ewok_session:get_session(Request:cookies(), Request:remote_ip()),
%	?TTY({session, Session}),
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
	Route = #ewok_route{realm = Realm} ->
		Request:set_realm(Realm),
		get_access(Request, Session, Route);
	_ ->
		do_response(Request, Session, internal_server_error) %% TODO: shouldn't this be 404?
	end.

%% HOW
get_access(Request, Session, #ewok_route{handler=Module, realm=Realm, roles=Roles}) ->	
	case validate_access(Session:user(), Realm, Roles) of
	ok -> 
		case catch(handle_request(Request, Session, Module)) of
		ok -> 
			ok;
		{'EXIT', normal} -> 
			ok;
		Value -> 
			io:format("~p~n", [Value])
		end;
	not_authorized ->
		case ewok:config({Realm, http, login}) of
		undefined -> 
			ewok_log:warn([{auth_login_undefined, Realm}]),
			do_response(Request, Session, unauthorized);
		LoginPath ->
			Here =
				case Request:header(referer) of
				undefined -> 
					ewok_http:absolute_uri(Request:path());
				Value -> 
					Value
				end,
			Session:save(redirect, Here),
			% Location = ewok_http:absolute_uri(LoginPath),
			%% NOTE: traditionally, this is 'found 302' - but 'temporary_redirect 307' may be better
			do_response(Request, Session, {found, [{location, LoginPath}], []})
		end
	end.

%% WHAT
handle_request(Request, Session, Module) ->
	ewok_log:info([{call, {Request:path(), Module, Session:data()}}]),
	Response = 
		case code:ensure_loaded(Module) of
		{'module', Module} ->
			case Module:filter(Request) of
			{delegate, Delegate, Options} when is_atom(Delegate), is_list(Options) -> 
				ewok_log:info([{delegated, {Delegate, Options}}]),
				Session:save(delegated, Options),
				%% TODO: how to implement delegation Options for a handler without making
				%% the handler's client code onerous?
				handle_request(Request, Session, Delegate);
			ok ->
				invoke_service(Request, Session, Module);
			_ ->
				precondition_failed
			end;
		Error ->
			ewok_log:error(Error),
			internal_server_error
		end,
	do_response(Request, Session, Response).
	
%% NOTE: Everything else is just boilerplace compared to this...
invoke_service(Request, Session, Module) ->
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
			{ok, Headers, _} -> 
				{ok, Headers, []};
			Result -> 
				Result
			end;
		false ->
			method_not_allowed
		end
	end.

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
	SessionKey = Session:key(),
	NewHeaders =
		case proplists:get_value(?EWOK_SESSION_KEY, Request:cookies()) of
		SessionKey -> 
			Headers;
		_ -> 
			[{set_cookie, Session:cookie()}|Headers]
		end,
	Response = {response, Code, NewHeaders, Content, false},	
	%% and finally...
	{ok, _HttpResponse, BytesSent} = ewok_response:reply(Request, Response),
	%%
	{Tag, Message} = format_access_log(Request, Session, Code, BytesSent),
	ok = ewok_log:message(access, Tag, Message),
	%%
	cleanup(Request).

%%
cleanup(Request) ->
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
		?TTY({"Closing", Request:socket()}),
		ewok_log:info({closed, Request:socket()}),
		ewok_socket:close(Request:socket()),
		exit(normal);
	false ->
		case Request:websocket() of
		true ->
			%?TTY("Keeping WebSocket Open...~n", []),
			ok;
		_ ->
			%?TTY("Connection ~p~n", [Request:header(connection)]),
			%% NOTE: IE and Chrome (not Firefox) will close out when 304 not_modified is 
			%% returned even though they ask for keep-alive... weird! Chrome lists this 
			%% as a fixed bug in their browser, which again leaves IE as the toy.
			Timeout = Request:timeout(), 
			Request:reset(),
			?MODULE:service(Request:socket(), Timeout, Request:max_headers())
		end
	end.

%%
%% INTERNAL
%%

%%
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

%%
lookup_route(Path) ->
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
	Route = #ewok_route{} -> 
		Route;
	undefined -> 
		lookup_wildcard_route(T);
	Error -> 
		Error
	end;
lookup_wildcard_route([]) ->
	case ewok_db:lookup(ewok_route, default) of
	Route = #ewok_route{} -> 
		Route;
	undefined -> 
		{error, no_handler};
	Error -> 
		Error
	end.
%
wildcard_path(Parts) ->
	list_to_binary([[<<$/>>, X] || X <- lists:reverse([<<$*>>|Parts])]).

%% TODO: move to ewok_identity/ewok_auth... ** i.e. make pluggable
validate_access(_, _, any) -> 
	ok;
validate_access(undefined, _, _) -> 
	not_authorized;
validate_access(#ewok_user{roles = UserRoles}, Realm, ResourceRoles) ->
	Auth = lists:map(fun (R = {_, _}) -> R; (R) -> {Realm, R} end, ResourceRoles),
	case [X || X <- UserRoles, Y <- Auth, X =:= Y] of
	[] -> 
		not_authorized;
	_ -> 
		ok
	end.

%% TODO: Move this later on. To...?
format_access_log(Request, Session, StatusCode, BytesSent) ->
	UserId = 
		case Session:user() of
		#ewok_user{name = Name} -> 
			list_to_binary(io_lib:format("~p", [Name]));
		_ -> 
			<<"{-,-}">>
		end,
		
	{Major, Minor} = Request:version(),
	Version = list_to_binary(io_lib:format("HTTP/~w.~w", [Major, Minor])),
	Status = list_to_binary(integer_to_list(StatusCode)),
	UserAgent =
		case Request:header(<<"User-Agent">>) of
		undefined -> 
			<<"(No Header: User-Agent)">>;
		Value -> 
			Value
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

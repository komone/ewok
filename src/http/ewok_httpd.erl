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

-module(ewok_httpd).

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_inet).
-export([start/1, init/2, terminate/3]).
% FSM State Callbacks
-export([connected/2, websocket/2]).

%% Remove or change
-export([find_login/3, format_access_log/4]).

-record(state, {remote_ip, remote_port}).

%
start(Port) ->
	#ewok_inet{
		port = Port,
		transport = tcp,
		protocol = http,
		handler = ?MODULE,
		codec = ewok_http,
		timeout = 30
	}.

%% Callbacks: ewok_inet
init(_Options, {RemoteIP, RemotePort}) ->
	{noreply, connected, #state{remote_ip = RemoteIP, remote_port = RemotePort}}.
	
% FSM Callbacks
connected(Request, State) ->
	Response = handle_request(Request, State), 
	case Response of
	#http_response{close = false} ->
		{reply, Response, connected, State};
	_ ->
		{reply, Response, terminate, State}
	end.

websocket(_Request, State) ->
	{reply, ok, websocket, State}.
	
%
terminate(_Reason, _NextState, _StateData) ->
	ok.

%%
%% Internal
%%
handle_request(Request = #http_request{version = Version, headers = Headers}, #state{remote_ip = RemoteIP}) ->
	Cookies = ewok_http_cookie:parse_cookies(Request),
	Request0 = Request#http_request{ip = RemoteIP, cookies = Cookies},	
	ewok_log:info({?MODULE, Request0}),
	Session = ewok_session:get_session(Cookies, RemoteIP),
%	NewCookie = ewok_session:make_cookie(Session#http_session.key),
	Response = 
		case Version of
		{1, 1} ->
			route(Request0, Session);
		{1, 0} ->
			route(Request0, Session);
		_ ->
			ewok_log:warn([{http_protocol, Version}, 
				{user_agent, proplists:get_value(user_agent, Headers)}]),
			http_version_not_supported	
		end,
	response(Request0, Session, Response);
handle_request(_, _) ->
	?TTY({handle_request, internal_server_error}),
	internal_server_error.

%% WHERE
route(R = #http_request{path = Path}, Session) ->
	case ewok_http_router:lookup(Path) of
	Route = #ewok_route{realm = Realm} ->
		access(R#http_request{realm = Realm}, Session, Route);
	_ ->
		?TTY({route, internal_server_error}),
		internal_server_error %% TODO: should this be 404?
	end.

%% HOW
access(Request, Session = #http_session{user = User}, Route) ->	
	case ewok_security:check_access(User, Route) of 
	ok ->
		process_request(Request, Session, Route);
	{error, _} ->
		?TTY({access, not_authorized}),
		unauthorized
	end.

process_request(Request, Session, #ewok_route{handler = Handler}) ->
	try invoke(Request, Session, Handler) of
	Value ->
		%?TTY({process_request, Handler}),
		Value
	catch
	E:R ->
		ewok_log:error({http_error, {E, R}, [Request, Session, Handler]}),
		?TTY({process_request, Handler, {E, R}}),
		internal_server_error
	end.
	
%% WHAT
invoke(Request, Session, Handler) ->
	ewok_log:info([{call, {Request#http_request.path, Handler, Session#http_session.data}}]),
	case code:ensure_loaded(Handler) of
	{module, Handler} ->
		invoke_filter(Request, Session, Handler);
	Error ->
		ewok_log:error(Error),
		?TTY({invoke, Handler, Error}),
		internal_server_error
	end.
	
invoke_filter(Request, Session = #http_session{data = Data}, Handler) ->
	case Handler:filter(Request) of
	{delegate, Delegate, Options} when is_atom(Delegate), is_list(Options) -> 
		ewok_log:info([{delegated, {Delegate, Options}}]),
		Session0 = Session#http_session{data = [{delegated, {Delegate, Options}}|Data]},
		ewok_session:update(Session0),
		%% TODO: how to implement delegation Options for a handler without making
		%% the handler's client code onerous?
		invoke(Request, Session0, Delegate);
	ok ->
		invoke_method(Request, Session, Handler);
	_ ->
		precondition_failed
	end.
	
%% NOTE: Everything else is just boilerplace compared to this...
invoke_method(Request = #http_request{method = Method}, Session, Handler) ->
%	?TTY({Path, Handler}),
	%% TODO - we may wish to trap 'OPTIONS' here
	case erlang:function_exported(Handler, Method, 2) of
	true -> 
		Handler:Method(Request, Session);
	false ->
		%% IMPL: if possible, autogenerate a response for 'HEAD'
		case Method =:= 'HEAD' 
			andalso erlang:function_exported(Handler, 'GET', 2) of
		true -> 
			case Handler:'GET'(Request, Session) of
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
response(Request, Session, Status) when is_atom(Status); is_integer(Status) ->
	response(Request, Session, {Status, []});
%
response(Request, Session, {Status, Headers}) when is_atom(Status); is_integer(Status) ->
	response(Request, Session, {Status, Headers, ewok_http:status_message(Status)});
%
response(Request = #http_request{version = Version, cookies = Cookies}, Session, {Status, Headers, Content}) ->
	Code = ewok_http:status_code(Status),
	case ewok_session:update(Session, Cookies) of
	{ok, NewCookie} ->
		%?TTY({set_cookie, NewCookie}),
		Headers0 = [{set_cookie, NewCookie}|Headers];
	ok -> 
		Headers0 = Headers
	end,
	Connection = proplists:get_value(connection, Request#http_request.headers),
	Close = 
		case {Version, Connection} of
		{{1, 1}, <<"close">>} ->
			true;
		{{1, 1}, _} ->
			false;		
		{{1, 0}, <<"keep-alive">>} ->
			false;
		_ ->
			true
		end,
%	{Tag, Message} = format_access_log(Request, Session, Code, BytesSent),
%	ok = ewok_log:message(access, Tag, Message),
	#http_response{status = Code, headers = Headers0, content = Content, close = Close}.
	
%%
%% INTERNAL
%%

%% should this be in ewok_security?
find_login(Request, Session, #ewok_route{realm = Realm}) ->
	case ewok:config({Realm, http, login}) of
	undefined -> 
		ewok_log:warn([{auth_login_undefined, Realm}]),
		not_authorized;
	LoginPath ->
		Here =
			case proplists:get_value(referer, Request#http_request.headers) of
			undefined -> 
				ewok_http:absolute_uri(Request#http_request.path);
			Value -> 
				Value
			end,
		% Location = ewok_http:absolute_uri(LoginPath),
		%% NOTE: traditionally, this is 'found 302' - but 'temporary_redirect 307' may be better
		{Request, Session#http_session{data = [{redirect, Here}]}, {found, [{location, LoginPath}], []}}
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

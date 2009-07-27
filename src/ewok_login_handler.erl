%%
-module(ewok_login_handler).
-vsn("1.0").
-author('steve@simulacity.com').

-include("ewok.hrl").
-include("esp.hrl").

-behavior(ewok_http_resource).
-export([filter/1, resource_info/0]).
-export(['GET'/2, 'POST'/2, get_form/3]).
%%
%% Resource Callbacks
%%
resource_info() -> [{name, "Ewok Login Handler"}].

%%
filter(_Request) ->  
	ok.
	
%%
'GET'(Request, Session) ->
%	?TTY("Login ~p~n", [{Request, Session}]),
	PageSpec = [
		{title, <<"Ewok AS - Login">>},
		{menu, [#p{body=[<<"A Web Application Server written in Erlang/OTP">>]}]},
		{content, [
			#h1{body=[<<"Login">>]},
			#p{body=[<<"Please log in using the details provided to you. ">>,
			<<"If you have forgotten your username or password, or do not ">>,
			<<"yet have a login, please contact your support representative ">>,
			<<"to enable this account.">>]},
			get_form(Request, Session, [])
		]}
	],
	ewok_web:render(Request, Session, PageSpec).

%%
'POST'(Request, Session) ->
	Realm = Request:parameter("realm"), %% hidden
	URL = Request:parameter("authorize"), %% hidden
	Username = Request:parameter("username"),
	Password = Request:parameter("password"),
	%%	
	%?TTY("LOGIN ~p~n", [{Realm, Username}]),
	case esp:validate([Realm, URL, Username, Password]) of
	true ->
		case ewok_users:login(Realm, Username, Password) of 
		{ok, User} when is_record(User, user) -> 
			Session:user(User),
			do_auth_log(Request, Session, URL),
			{found, [{location, URL}], []};
		E = {error, not_activated} ->
			?TTY("DENIED ~p~n", [E]), 
			%% do more with this later
			precondition_failed;
		E = {error, _} ->
			?TTY("DENIED ~p~n", [E]), 
			%% do more with this later
			unauthorized
		end;
	false ->
		bad_request %% the right thing?
	end.

%%
get_form(Request, Session, Opts) ->
	UsernameLabel = proplists:get_value(username, Opts, <<"Username: ">>),
	PasswordLabel = proplists:get_value(password, Opts, <<"Password: ">>),
	Redirect = 
		case Session:take(redirect) of
		{redirect, URL} -> URL;
		undefined -> ewok_http:absolute_uri("/")
		end,
	#form{method=post, action=Request:path(), body=[
		#input{type=hidden, name="authorize", value=Redirect},
		#input{type=hidden, name="realm", value=Request:realm()},%
		#grid{body=[
			[UsernameLabel, #input{type=text, name="username"}],
			[PasswordLabel, #input{type=password, name="password"}],
			[<<>>, #input{type=submit}]
		]}
	]}.

%%
do_auth_log(Request, Session, URL) ->
	UserId = 
		case Session:user() of
		User when is_record(User, user) -> 
			esp_html:text(User#user.name);
		_ -> <<"{-,-} ">>
		end,
	Tag = <<"login">>,
	Line = list_to_binary([
		Request:remote_ip(), <<" ">>,
		UserId, <<" ">>,
		Session:key(), <<" ">>, 
		URL, <<" ">>,
		ewok_http:browser_detect(Request:header(<<"User-Agent">>))
	]),
	ewok_log:log(auth, Tag, Line).

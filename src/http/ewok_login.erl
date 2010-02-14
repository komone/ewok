%%
-module(ewok_login).
-vsn("1.0").
-author('steve@simulacity.com').

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("esp.hrl").

%%
-behavior(ewok_http_resource).
-export([filter/1, do/3, resource_info/0]).

%%
resource_info() -> [{name, "Ewok Login Handler"}].

%%
filter(_Request) ->
	ok.

%%
do('GET', Request, Session) ->
	Redirect = 
		case Session:take(redirect) of
		{redirect, URL} -> URL;
		undefined -> ewok_http:absolute_uri("/")
		end,
	Realm = Request:get_realm(),
	Form =
		#form{method=post, action=Request:path(), body=[
			#input{type=hidden, name="authorize", value=Redirect},
			#input{type=hidden, name="realm", value=Realm},%
			#grid{body=[
				[<<"Username: ">>, #input{type=text, name="username"}],
				[<<"Password: ">>, #input{type=password, name="password"}],
				[<<>>, #input{type=submit}]
			]}
		]},
	Form;
%	page(Spec, Request, NewSession); %%

%%
do('POST', Request, Session) ->
	URL = %% hidden
		case Request:parameter("authorize") of 
		undefined -> ewok_http:absolute_uri("/");
		Value -> Value
		end,
	Realm = Request:parameter("realm"), %% hidden	
	Username = Request:parameter("username"),
	Password = Request:parameter("password"),
	ewok_log:info(io_lib:format("LOGIN ~p~n", [{Realm, Username}])),
	%%
	case ewok_users:login(Realm, Username, Password) of 
	{ok, #ewok_user{} = User} -> 
		Session:user(User),
		do_auth_log(Request, Session, URL),
		{found, [{location, URL}], []};
	E = {error, not_activated} ->
		ewok_log:info(io_lib:format("DENIED ~p~n", [E])), 
		%% do more with this later
		precondition_failed;
	E = {error, _} ->
		ewok_log:info(io_lib:format("DENIED ~p~n", [E])), 
		%% do more with this later
		unauthorized
	end.

%%
do_auth_log(Request, Session, URL) ->
	UserId = 
		case Session:user() of
		#ewok_user{} = User -> 
			list_to_binary(io_lib:format("~p", [User#ewok_user.name]));
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
	ewok_log:message(auth, Tag, Line).

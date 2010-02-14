%%
-module(ewok_activation_handler).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("esp.hrl").

-behaviour(ewok_http_resource).
-export([filter/1, resource_info/0]).

-export(['GET'/2, 'POST'/2]).

%%
%% Resource Callbacks
%%
resource_info() -> [].

%%
filter(_Request) ->  ok.

%%
'GET'(Request, Session) ->
	%	?TTY("Activate ~p~n", [{Request, Session}]),
	PageSpec = 
		case Session:user() of 
		undefined -> page1(Request);
		_ -> page3()
		end,
	ewok_web:render(Request, Session, PageSpec). %% NOTE render -> response().

%%
'POST'(Request, Session) ->
	%?TTY(Session:read(activation)),
	case Session:take(activation) of
	undefined ->
		Realm = Request:parameter("realm"), %% hidden
		Username = Request:parameter("username"),
		Activation = Request:parameter("activation"),
		case esp:validate([Realm, Username, Activation], not_null) of
		true -> 
			PageSpec = 
				case ewok_users:exists(Realm, Username) of
				true -> 
					Session:save(activation, {Realm, Username, Activation}),
					page2(Request, Username);
				false ->
					page3()
				end,
			ewok_web:render(Request, Session, PageSpec);
		false ->
			'GET'(Request, Session)
		end;
	{activation, {Realm, Username, Activation}} ->
		Password = Request:parameter("password"),
		Password2 = Request:parameter("password2"),
		case esp:validate([Password, Password2], not_null) 
			andalso Password =:= Password2 of
		true ->
			case ewok_users:activate(Realm, Username, 
					list_to_binary(Activation), Password) of 
			{ok, #ewok_user{} = User} -> 
				?TTY({"USER", User}),
				Session:user(User),
				{found, [{location, ewok_http:absolute_uri("/")}]};
			E = {error, __} ->
				?TTY({"DENIED", E}), 
				%% do more with this later
				unauthorized
			end;
		false ->
			precondition_failed
		end;		
	_ ->
		internal_server_error
	end.

%%
page1(Request) -> [
	{title, <<"Ewok - Activation">>},
	{menu, [#p{body=[<<"A Web Application Server written in Erlang/OTP">>]}]},
	{content, [
		#h1{body=[<<"Activation">>]},
		#p{body=[<<"Please use the activation key provided to you. ">>,
		<<"If you have lost this key or do not have it, ">>,
		<<"please contact your support representative.">>]},
		#form{method=post, action=Request:path(), body=[
			#input{type=hidden, name="realm", value=Request:realm()}, 
			#grid{body=[
				[<<"Username: ">>, #input{type=text, name="username"}],
				[<<"Activation Key: ">>, #input{type=text, name="activation"}],
				[<<>>, #input{type=submit}]
			]}
		]}
	]}
].

%%
page2(Request, Username) -> [
	{title, <<"Ewok - Activation">>},
	{menu, [#p{body=[<<"A Web Application Server written in Erlang/OTP">>]}]},
	{content, [
		#h1{body=[<<"Choose a password">>]},
		#p{body=[<<"Please use the activation key provided to you. ">>,
		<<"If you have lost this key or do not have it, ">>,
		<<"please contact your support representative.">>]},
		#form{method=post, action=Request:path(), body=[
			#grid{body=[
				[<<"Username:">>, Username],
				[<<"Choose a password: ">>, #input{type=password, name="password"}],
				[<<"Repeat the password: ">>, #input{type=password, name="password2"}],
				[<<>>, #input{type=submit}]
			]}
		]}
	]}
].

page3() -> [
	{title, <<"Ewok - Activation">>},
	{menu, [#p{body=[<<"A Web Application Server written in Erlang/OTP">>]}]},
	{content, [
		#h1{body=[<<"Activation">>]},
		#p{body=[<<"This account is already active.">>]}
	]}
].

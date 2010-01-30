%%
-module(ewok_registration_handler).
-vsn("1.0").
-author('steve@simulacity.com').

-include("../include/ewok.hrl").
-include("../include/esp.hrl").

-behaviour(ewok_http_resource).
-export([resource_info/0, filter/1, 'GET'/2, 'POST'/2]).

%%
%% Resource Callbacks
%%
resource_info() -> [{name, "Ewok Registration Handler"}].

%%
filter(_Request) ->  
	ok.
%%
'GET'(Request, Session) ->
%	?TTY("Registration ~p~n", [{Request, Session}]),
	Spec = [
		{title, <<"Ewok - Registration">>},
		{menu, [#p{body=[<<"A Web Application Server written in Erlang/OTP">>]}]},
		{content, [
			#h1{body=[<<"Registration">>]},
			#p{body=[<<"Please choose a user name.">>]},
			#form{method=post, action=Request:path(), body=[
				#input{type=hidden, name="domain", value="ewok"}, %% PARAMETERIZE THIS!
				#grid{body=[
					[<<"Username: ">>, #input{type=text, name="username"}],
					[<<"Choose a password: ">>, #input{type=password, name="password"}],
					[<<"Repeat the password: ">>, #input{type=password, name="password2"}],
					[<<>>, #input{type=submit}]
				]}
			]}
		]}
	],
	ewok_web:render(Request, Session, Spec). %% later generalize on realm/domain
%
'POST'(Request, Session) ->
	Domain = Request:parameter("domain"), %% hidden
	Username = Request:parameter("username"),
	Password = Request:parameter("password"),
	Password2 = Request:parameter("password2"),

	?TTY({"REGISTRATION", {Domain, Username, Password}}),
	case Password =:= Password2 of
	true ->
		case ewok_users:create_user(Domain, Username, Password) of 
		{ok, Activation} -> 
			?TTY({"ACCEPTED", Activation}),
			Spec = success(Request, Session, Activation),
			ewok_web:render(Request, Session, Spec); %% later generalize on realm/domain			
		E = {error, __} ->
			?TTY({"DENIED", E}), 
			%% do more with this later
			'GET'(Request, Session)
		end;
	false ->
		precondition_failed
	end.

success(_Request, _Session, Activation) -> [
	{title, <<"Ewok - New User">>},
	{menu, [#p{body=[<<"A Web Application Server written in Erlang/OTP">>]}]},
	{content, [
		#h1{body=[<<"Success">>]},
		#grid{body=[
			[<<"Your Activation Key: ">>, #span{class="hilite", body=[Activation]}]
		]},
		#p{body=[<<"You may now ">>, #a{href="/login", body=[<<"Log in">>]}, <<".">>]}
	]}
].

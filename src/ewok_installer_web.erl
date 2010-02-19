%% Copyright 2009 Steve Davis <steve@simulacity.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ewok_installer_web).
-name("Ewok Installer Activation").

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("esp.hrl").

-behaviour(ewok_http_resource).
-export([filter/1, resource_info/0]).
-export(['GET'/2, 'POST'/2]).

%%
%% ewok_http_resource callbacks
%%
resource_info() -> [].

%%
filter(_Request) ->  ok.

%%
'GET'(Request, Session) ->
	case Session:user() of
	undefined ->
		ewok_web:render(Request, Session, page1(Request, <<>>));
%	#ewok_user{id=ID, name=?ADMIN_USER} ->
%		ewok_web:render(Request, Session, page2(Request, <<>>));
	_ ->
		unauthorized
	end.
	
%% TODO: change to use esp:validate instead of direct use of validator
'POST'(Request, Session) ->
	case Session:read(activation) of
	undefined ->
		Realm = Request:parameter(<<"realm">>),
		Username = Request:parameter(<<"username">>),
		Activation = Request:parameter(<<"activation">>),
		case esp_validator:not_null([Realm, Username, Activation]) of
		false ->
			ewok_web:render(Request, Session, page1(Request, <<"Enter the Activation Key">>));
		true ->
			Session:save(activation, {Realm, Username, Activation}),
			ewok_web:render(Request, Session, page2(Request, <<>>))
		end;
	{activation, {Realm, Username, Activation}} ->	
		Password = Request:parameter(<<"password">>),
		Password2 = Request:parameter(<<"password2">>),
		case esp_validator:not_null([Password, Password2]) of
		true -> 
			case Password =:= Password2 of
			true ->
				?TTY({params, Realm, Username, Activation, Password}),
				Session:take(activation),
				case ewok_users:activate(Realm, Username, Activation, Password) of 
				{ok, #ewok_user{} = User} ->
					?TTY({"USER", User}),
					Session:user(User),
					%% Activated == true, so...
					
					% TODO: Reconsider this, we should not
					% be calling back into ewok_app
					ewok_app:deploy_web(true),
					
					{found, [{location, ewok_http:absolute_uri("/")}]};
				E = {error, _} ->
					?TTY({"DENIED", E}), 
					%% do more with this later
					unauthorized
				end;
			false ->
				ewok_web:render(Request, Session, page2(Request, <<"Passwords do not match">>))
			end;
		false ->
			ewok_web:render(Request, Session, page2(Request, <<"Complete all parts of the form">>))
		end
	end.

%%
page1(Request, Flash) -> 
	{Realm, Username} = ?ADMIN_USER,
	[{title, <<"Ewok - Installer">>},
	 {menu, [#p{body = [<<"A Web Application Server written in Erlang/OTP">>]}]},
	 {dock, []},
	 {content, [
		#h1{body = [<<"Activation">>]},
		#p{body = [<<"Please use the administrator activation key provided.">>,
		<<"If you have lost this key or do not have it, please review the ">>,
		<<"server documentation.">>]},
		#form{method = post, action = Request:path(), body = [
			#input{type = hidden, name = "realm", value = esp_html:text(Realm)},
			#input{type = hidden, name = "username", value = esp_html:text(Username)},
			#grid{caption = [Flash], body = [
				[<<"Enter Activation Key: ">>, #input{type = text, name = "activation"}],
				[<<>>, #input{type = submit}]
			]}
		]}
	]}].

%%
page2(Request, Flash) -> 
	{_Realm, Username} = ?ADMIN_USER,
	[{title, <<"Ewok - Installer">>},
	 {menu, [#p{body = [<<"A Web Application Server written in Erlang/OTP">>]}]},
	 {dock, []},
	 {content, [
		#h1{body = [<<"Activation">>]},
		#p{body = [<<"Please choose an administrator password.">>,
		<<"If you have lost this key or do not have it, please review the ">>,
		<<"server documentation.">>]},
		#form{method = post, action = Request:path(), body = [
			#grid{caption = [Flash], body = [
				[<<"Administrator Username: ">>, esp_html:text(Username)],
				[<<"Choose a password: ">>, #input{type = password, name = "password"}],
				[<<"Repeat the password: ">>, #input{type = password, name = "password2"}],
				[<<>>, #input{type = submit}]
			]}
		]}
	]}].

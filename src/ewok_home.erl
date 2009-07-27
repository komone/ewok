-module(ewok_home).
-vsn("1.0").
-author('steve@simulacity.com').

-include("ewok.hrl").
-include("esp.hrl").

-behavior(ewok_http_resource).
-export([filter/1, resource_info/0]).
-export(['GET'/2]).

%%
%% Resource Callbacks
%%
resource_info() -> [{name, "Ewok Home Page"}].

%%
filter(_Request) ->  
	ok.
	
%%
'GET'(Request, Session) ->
	PageSpec = [
		{title, <<"Ewok AS">>},
		{menu, [#p{body=[<<"A Web Application Server written in Erlang/OTP">>]}]},
		{content, [
			#h1{body=[<<"Welcome">>]},
			#p{body=[<<"Write an intro!">>]}
		]}
	],
	ewok_web:render(Request, Session, PageSpec).

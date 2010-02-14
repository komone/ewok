%%%% Copyright 2010 Steve Davis <steve@simulacity.com>
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

-module(ewok_home).

-include("ewok.hrl").
-include("esp.hrl").

-behaviour(ewok_http_resource).
-export([filter/1, resource_info/0]).
-export(['GET'/2]).

-interface([
	{resource_info/0, 'behaviour', "See ewok_http_resource", "void() -> list()"},
	{filter/1, 'behaviour', "See ewok_http_resource", "term() -> ok | term()"},
	{'GET'/2, api, "See ewok_http_resource", "term(), term() -> atom() | term()"}
]).
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

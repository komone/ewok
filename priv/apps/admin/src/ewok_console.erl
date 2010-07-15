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

-module(ewok_console).
-include("ewok.hrl").
-include("ewok_system.hrl").
-include("esp.hrl").

-behaviour(ewok_http_resource).
% http_resource callbacks
-export([filter/1, resource_info/0]).

-export(['GET'/2]).

%%
%% Resource Callbacks
%%
resource_info() -> 
	[{name, "Ewok Administration Console"}].

%%
filter(_Request) ->  
	ok.

'GET'(_Request, Session) ->
	esp:render(#page{title="Ewok Admin", head=head(), body=body(Session)}).

head() -> [
	#link{rel="icon", href="/favicon.png", type="image/png"},
	#css{src="css/jquery-ui-1.8.1.custom.css"},
	#css{src="esp.css"},
	#script{src="js/jquery-1.4.2.min.js"},
	#script{src="js/jquery-ui-1.8.1.custom.min.js"},		
	#script{src="js/jquery-jeditable-1.7.1.min.js"},
	#script{src="ewok-admin.js"},
	#script{body=[<<"$(function(){$(\"#dock\").buttonset().change(function(event){">>,
		<<"$(\"#page\").load(event.target.id + \".html\");});$(\"#page\").load(\"welcome.html\");});">>]}].

body(Session) ->
	Username = 
		case Session#http_session.user of
		undefined -> 
			<<"Guest">>;
		U = #ewok_user{} -> 
			{_, Name} = U#ewok_user.name,
			list_to_binary(Name)
		end,
	[ 	#'div'{id="header", body = [
			dock(Username),
			#img{id="logo", src= <<"/images/ewok-logo.png">>}
		]},
		#'div'{id="page"},
		#'div'{id="footer", body=[<<"&copy; Simulacity.com">>]}
	].

dock(Username) ->
	Type = <<"radio">>,
	Name = <<"radio">>,
	#form{id="dock", body = [
		#span{id="user", body=[Username]},
		#input{id="dashboard", name=Name, type=Type},
		#label{for="dashboard", body = [<<"Dashboard">>]},
		#input{id="news", name=Name, type=Type},
		#label{for="news", body = [<<"News">>]},
		#input{id="reference", name=Name, type=Type},
		#label{for="reference", body = [<<"Reference">>]},
		#input{id="about", name=Name, type=Type},
		#label{for="about", body = [<<"About">>]}
	]}.

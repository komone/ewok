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

-module(ewok_admin).

-include_lib("ewok/include/ewok.hrl").
-include_lib("ewok/include/ewok_system.hrl").
-include_lib("ewok/include/esp.hrl").

-behaviour(ewok_web_application).
-export([application_info/0]).
-compile(export_all).

application_info() -> [
	{name, "Ewok Web Administration"}, 
	{version, {1,0,0}}
].

%
init([]) ->
	ok.
	
%
page(Spec, _Request, Session) ->
	esp:render(#page{
		title = proplists:get_value(title, Spec, <<"Ewok AS">>),
		head = [
			#css{src = "/default.css"},
			#link{rel = "icon", href = "/favicon.png", type = "image/png"},
			proplists:get_value(head, Spec, [])
		],
		body = [
			#'div'{id="top", body=[
				#img{id = "logo", src = "/images/ewok-logo.png"},
				#'div'{id = "dock", body = dock(Session)}
			]},
			#'div'{id = "page", body = [
				#'div'{id = "nav", body = proplists:get_value(menu, Spec, [])},
				#'div'{id = "content", body = proplists:get_value(content, Spec, [])}
			]},
			#br{clear = "all"},
			#'div'{id = "footer", body = [
				#hr{},
				#p{body = [<<"Copyright &copy; 2009 Simulacity.com. All Rights Reserved.">>]}
			]}
		]
	}).

%
dock(Session) ->
	Username = 
		case Session:user() of
		undefined -> <<"Guest">>;
		U = #ewok_user{} -> 
			{_, Name} = U#ewok_user.name,
			list_to_binary(Name)
		end,
	[#span{class="label", body=[
		Username, 
		<<" | ">>,
		#a{href="/admin", body=[<<"Dashboard">>]},
		<<" | ">>,
		#a{href="/", body=[<<"News">>]},
		<<" | ">>,
		#a{href="/doc", body=[<<"Documentation">>]},
		<<" | ">>,
		#a{href="/", body=[<<"About">>]}
	]}].

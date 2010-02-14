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

-module(ewok_app).

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(application).
-export([start/2, config_change/3, prep_stop/1, stop/1]).
-export([deploy_web/1]).

%% 
start(normal, Services) ->
	% force reload of ewok.app config file
	application:unload(ewok),
	
	% IMPL: Maybe use this later -- for now ignore
    {ok, _IP} = 
		case os:getenv("EWOK_IP") of 
		false -> {ok, {0,0,0,0}};
		Any -> inet_parse:address(Any) 
		end,	
	% Consider writing a .pid file
	OsPid = os:getpid(),
	
	% ensure boot logging is available
	ok = ewok_logging_srv:start_link([]),
	
	%
	ewok_log:message(?MODULE, ewok:ident()),	
	ewok_log:message(?MODULE, [{os, os:type(), {pid, OsPid}}]),
	{ok, Host} = inet:gethostname(),
	{ok, HostEnt} = inet:gethostbyname(Host),
	ewok_log:message(?MODULE, [{inet, {node, node()}, HostEnt}]),	
	ewok_log:message(?MODULE, [{loaded, X} || X <- application:loaded_applications()]),
	
	%
	Autoinstall = ewok_config:get_env(autoinstall, true),
	case ewok_installer:run(Autoinstall) of
	ok ->
		start_server(Services, true);
	{ok, AdminUser, Activation} ->
		ewok_log:message(?MODULE, {activation, AdminUser, Activation}),
		io:format(user, "~p~n", [{AdminUser, Activation}]),
		start_server(Services, false);
	Error ->
		Error
	end.
%% 
config_change(Changed, New, Removed) -> 
	?TTY({config_change, {changed, Changed}, {new, New}, {removed, Removed}}),
	ok.
%% 
prep_stop(State) ->
	State.
%% 
stop(_State) ->
	ok.

%% @private
start_server(Services, Activated) ->
	{ok, Datasource} = ewok_db:start(),
	ewok_log:message(?MODULE, Datasource),

	% TODO: this is questionable
	ewok_config:load_mimetypes(),
	
	ewok_log:message(?MODULE, {services, Services}),
	
	case supervisor:start_link({local, ewok_sup}, ewok_sup, Services) of
	{ok, Pid} ->
		%% swap out and remove boot log
		ewok_log:add_log(server),
		ewok_log:set_default(server),
		ewok_log:remove_log(boot),
		
		deploy_web(Activated),
		
		ewok_log:message(bootstrap, Pid),
		{ok, Pid};
	Error ->
		Error
	end.

%% TODO: here we should really treat the default ewok web config as a deployable web_app
%
deploy_web(false) ->
	Installer = 
		#web_app {
			id = ewok_installer, 
			path = ewok_util:appdir(),
			config = [
				{route, "/", ewok_installer_web, ewok, any},
				{route, default, ewok_file_handler, ewok, any}
			],
			valid = true,
			deployed = false
		},
	ewok_deployment_srv:load(Installer),
	ewok_deployment_srv:deploy(ewok_installer);
%
deploy_web(true) ->
	% TODO: Remove this temporary hack and undeploy the installer instead
	ok = ewok_db:delete({ewok_route,<<"/">>,ewok_installer_web,ewok,any}),
	
	case ewok_config:get_env(web_app) of
	undefined ->
		[];
	WebConfig ->
		ewok_config:load(ewok, http, WebConfig)
	end,
	
	AppList = ewok_config:get_env(autodeploy, []),
	ewok_log:info({autodeploy, AppList}),
	
	case whereis(ewok_deployment_srv) of
	P when is_pid(P) ->
		Results = [ewok_deployment_srv:deploy(App) || App <- AppList],
		case lists:dropwhile(fun(X) -> X =:= ok end, Results) of
		[] -> 
			ok;
		Errors -> 
			ewok_log:error({autodeploy, Errors})
		end;
	undefined ->
		ewok_log:error({autodeploy, {not_running, ewok_deployment_srv}})
	end.

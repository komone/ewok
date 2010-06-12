%% Copyright 2009-2010 Steve Davis <steve@simulacity.com>
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
-export([start/2, start_phase/3, config_change/3, prep_stop/1, stop/1]).
-export([deploy_web/1]).

%% 
start(_Type, AppOpts) ->
	EnvOpts = application:get_all_env(),
	% ensure boot logging is available
	ok = ewok_logging_srv:start_link(EnvOpts),
	
	% Consider writing a .pid file
	OsPid = os:getpid(),	
	ewok_log:message(?MODULE, ewok:ident()),	
	ewok_log:message(?MODULE, [{os, os:type(), {pid, OsPid}}]),
	ewok_log:message(?MODULE, [{loaded, X} || X <- application:loaded_applications()]),	
	
	Files = proplists:get_value(config_files, EnvOpts, []),
	FileOpts = ewok_config:load(Files),
	AllOpts = lists:append([AppOpts, EnvOpts, FileOpts]),
	
	% start config database
	ok = ewok_db:start(AllOpts),
	{ok, Pid} = supervisor:start_link({local, ewok_sup}, ewok_sup, []),
	
	% initial configuration of inet
	% IMPL: Maybe use this later -- for now ignore
    {ok, _IP} = 
		case os:getenv("EWOK_IP") of 
		false -> {ok, {0,0,0,0}};
		Any -> inet_parse:address(Any) 
		end,	
	{ok, Host} = inet:gethostname(),
	{ok, HostEnt} = inet:gethostbyname(Host),
	ewok_log:message(?MODULE, [{inet, {node, node()}, HostEnt}]),
	
	ok = ewok_sup:start_services(AppOpts),
	
	%% swap out and remove boot log
	ewok_log:add_log(server),
	ewok_log:set_default(server),
	ewok_log:remove_log(boot),
	
	{ok, Pid}.
%	{ok, AdminUser, Activation} ->
%		ewok_log:message(?MODULE, {activation, AdminUser, Activation}),
%		io:format(user, "~p~n", [{AdminUser, Activation}])
%	end,
	
%%
start_phase(core, _Type, Services) ->
	%% NOTE: {supervisor, ewok_data_sup}
	?TTY({start_phase, core, Services}),
	ewok_log:message(?MODULE, {core, Services}),
	{ok, Datasource} = ewok_db:start(),
	ewok_log:message(?MODULE, Datasource),
	ewok_sup:start_services(Services);
%%
start_phase(extensions, _Type, Services) ->
	?TTY({start_phase, extensions, Services}),
	ewok_log:message(?MODULE, {extensions, Services}),
	ewok_sup:start_services(Services);
%%
start_phase(inet, _Type, Services) ->
	?TTY({start_phase, inet, Services}),
	% IMPL: Maybe use this later -- for now ignore
    {ok, _IP} = 
		case os:getenv("EWOK_IP") of 
		false -> {ok, {0,0,0,0}};
		Any -> inet_parse:address(Any) 
		end,	
	{ok, Host} = inet:gethostname(),
	{ok, HostEnt} = inet:gethostbyname(Host),
	ewok_log:message(?MODULE, [{inet, {node, node()}, HostEnt}]),	
	ewok_config:load_mimetypes(),
	
	ok = ewok_sup:start_services(Services),
	
	%% swap out and remove boot log
	ewok_log:add_log(server),
	ewok_log:set_default(server),
	ewok_log:remove_log(boot),
	
	%% ewok_log:info({bootstrap, Pid}).
	%%	deploy_web(Activated),
	ok.
	
%% 
config_change(Changed, New, Removed) -> 
	?TTY({config_change, {changed, Changed}, {new, New}, {removed, Removed}}),
	ok.
	
%% 
prep_stop(State) ->
	?TTY({prep_stop, State}),
	%ewok_db:stop(),
	%% stop db?
	State.
	
%% 
stop(State) ->
	?TTY({stop, State}),
	ok.

%% TODO: here we should really treat the default ewok web config as a deployable web_app
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

%%
deploy_web(true) ->
	% TODO: Remove this temporary hack and undeploy the installer instead
	ok = ewok_db:delete({ewok_route, <<"/">>, ewok_installer_web, ewok, any}),
	
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


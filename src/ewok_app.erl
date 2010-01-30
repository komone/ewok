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

-module(ewok_app).

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(application).
-export([start/2, config_change/3, prep_stop/1, stop/1]).

%% 
start(normal, Services) ->
	%% force reload of ewok.app config file
	application:unload(ewok),
	
	%% IMPL: Maybe use this later -- for now ignore
    {ok, _IP} = 
		case os:getenv("EWOK_IP") of 
		false -> {ok, {0,0,0,0}};
		Any -> inet_parse:address(Any) 
		end,	
	% Consider writing a .pid file
	OsPid = os:getpid(),
	
	%% ensure boot logging is available
	ok = ewok_logging_srv:start_link(),
	
	%%
	ewok_log:message(?MODULE, ewok:ident()),	
	ewok_log:message(?MODULE, [{os, os:type(), {pid, OsPid}}]),
	{ok, Host} = inet:gethostname(),
	{ok, HostEnt} = inet:gethostbyname(Host),
	ewok_log:message(?MODULE, [{inet, {node, node()}, HostEnt}]),	
	ewok_log:message(?MODULE, [{loaded, X} || X <- application:loaded_applications()]),
	
	%%
	Autoinstall = ewok_config:get_env(autoinstall, true),
	case ewok_installer:run(Autoinstall) of
	{ok, Activation} ->
		{ok, Datasource} = ewok_db:start(),
		ewok_log:message(?MODULE, Datasource),
		ewok_log:message(?MODULE, {activation, Activation}),
		
		load_mimetypes(), 
		
		case start_services(Services) of
		{ok, Pid} ->
			%% swap out and remove boot log
			ewok_log:add_log(server),
			ewok_log:set_default(server),
			ewok_log:remove_log(boot),
			
			case ewok_config:get_env(web_app) of
			undefined ->
				ok;
			WebConfig ->
				ewok_config:load(ewok, http, WebConfig),
				autodeploy()
			end,
			
			ewok_log:message(bootstrap, Pid),
			{ok, Pid};
		Error ->
			Error
		end;
	Error ->
		Error
	end.
%
config_change(Changed, New, Removed) -> 
	?TTY({config_change, {changed, Changed}, {new, New}, {removed, Removed}}),
	ok.
%
prep_stop(State) -> 
	?TTY({prep_stop, State}),
	State.
%% 
stop(_State) ->
	%?TTY({state, State}),
	ok.
	
%% start_ewok(proplist()) -> {ok, pid()} | {error, Reason}
start_services(Services) ->
	ewok_log:message(?MODULE, {services, Services}),
	case supervisor:start_link({local, ewok_sup}, ewok_sup, Services) of
	{ok, Pid} ->
		{ok, Pid};
	Error -> 
		Error
	end.
	
load_mimetypes() ->
	F = fun (X, Y) ->
		X1 = 
			case X of 
			_ when is_atom(X) -> X;
			_ -> list_to_binary(X)
			end, 
		Y1 = list_to_binary(Y),
		{ewok_mimetype, X1, Y1}
		end,
	Mimetypes = ewok_config:get_env(mimetypes, []),
	Records = [F(K, V) || {K, V} <- Mimetypes],
	ewok_db:add(Records),
	ewok_log:message(?MODULE, [Records]).
	
%%
autodeploy() ->
	AppList = ewok:config({ewok, http, autodeploy}, []),
	ewok_log:info({autodeploy, AppList}),
	case whereis(ewok_deployment_srv) of
	P when is_pid(P) ->
		Results = [ewok_deployment_srv:deploy(App) || App <- AppList],
		case lists:dropwhile(fun(X) -> X =:= ok end, Results) of
		[] -> ok;
		Errors -> ewok_log:error({autodeploy, Errors})
		end;
	undefined ->
		ewok_log:error({autodeploy, {not_running, ewok_deployment_srv}})
	end.
	

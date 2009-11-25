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

-module(ewok).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl").

%% API
-export([start/0, start/1, stop/0]).
-export([ident/0]).
-export([info/1, config/1, config/2, configure/0, configure/1]).
-export([autodeploy/0, deploy/1, deploy/2, undeploy/1, undeploy/2]).

%% NOTE: consider an "embedded" mode startup?

%%
%% API
%% NOTE: try to make this semantically the same as application:start(ewok),
%% but allow non-stateful pre-launch checks.
start() ->
	%% NOTE: validate config file here so that the user sees a clear message
	%% in the console at start time...
	case ewok_configuration:preload(ewok) of
	{ok, _File, _Config} -> application:start(ewok);
	Error -> Error
	end.

%%
start(http) ->
	ok;
start(smtp) ->
	ok;
start(pop3) ->
	ok;
start(imap) ->
	ok;
start(amqp) ->
	ok;
start(ldap) ->
	ok;
start(dns) ->
	ok;
start(_) -> 
	error.

%%
stop() -> 
	application:stop(ewok).

%%
ident() ->
	list_to_binary([
		?SERVER_ID, 
		<<" build#">>, ewok_util:build_number(), 
		<<" [">>, ewok_util:build_time(), <<"]">>
	]).
	
% TOO: This will likely be radically revised over the course of development
info(Key) -> 
	case Key of
	version -> 
		[{Major, Minor, Patch}|_] = proplists:get_value('vsn', ?MODULE:module_info(attributes)),
		{Y, Mo, D, H, M, S} = proplists:get_value(time, ?MODULE:module_info('compile')),
		Build = calendar:datetime_to_gregorian_seconds({{Y, Mo, D}, {H, M, S}}),
		{Major, Minor, Patch, Build};
	running -> whereis(ewok_sup) =/= undefined;
	ports -> inet:i();
	config -> ewok_configuration:print();
	routes -> ewok_configuration:print(route);
	webapps -> 
		case whereis(ewok_deployment_srv) of
		undefined -> {error, not_started};
		Pid when is_pid(Pid) ->
			{_,_,_,Apps} = ewok_deployment_srv:list(),
			Apps
		end;
	users -> ewok_db:select(user);
	auth -> ewok_db:select(auth);
	roles -> ewok_db:select(role);
	sessions ->	 
		case whereis(ewok_session_srv) of 	
		undefined -> {error, not_started};
		_ -> 
			Sessions = ewok_session:list(),
			lists:foreach(fun(S) -> io:format("~p~n", [S]) end, Sessions),
			{ok, length(Sessions)}
		end;
	tasks ->
		Tasks = ewok_scheduler_srv:get_tasks(),
		io:format("** ~p: ~p active tasks...~n", [?MODULE, length(Tasks)]),
		lists:foreach(fun(T) -> io:format("  ~p~n", [T]) end, Tasks);
	_ -> 
		case code:ensure_loaded(Key) of
		{'module', Key} ->
			case erlang:function_exported(Key, service_info, 0) of
			true -> Key:service_info();
			false -> undefined
			end;
		_ -> undefined
		end
	end.

% TODO: improve naming config/configure
config(Property) ->
	ewok_configuration:get_value(Property).
%
config(Property, Default) ->
	ewok_configuration:get_value(Property, Default).
%	
configure() ->
	configure(ewok).
configure(App) ->
	ewok_configuration:load(App).

%%
autodeploy() ->
	AppList = 
		case ewok_installer:validate() of
		true ->
			ewok:config({ewok, http, autodeploy}, []);
		false ->
			% NOTE: placeholder... not fully implemented
			ewok:info(config),
			ewok:info(routes),
			[ewok_installer]
		end,
	ewok_log:info({autodeploy, AppList}),
	case whereis(ewok_deployment_srv) of
	P when is_pid(P) ->
		Results = [ewok_deployment_srv:deploy(App) || App <- AppList],
		case lists:dropwhile(fun(X) -> X =:= ok end, Results) of
		[] -> ok;
		Errors -> ewok_log:error({autodeploy, Errors})
		end;
	undefined ->
		ewok_log:warn({autodeploy, {not_running, ewok_deployment_srv}})
	end.

%%
deploy(App)->
	deploy(App, []).
deploy(App, Opts) when is_atom(App), is_list(Opts) ->
	case App of
	?MODULE -> {error, invalid_action};
	_ -> ewok_deployment_srv:deploy(App)
	end.
%%
undeploy(App) ->
	undeploy(App, []).
undeploy(App, Opts) when is_atom(App), is_list(Opts) ->
	case App of
	?MODULE -> {error, invalid_action};
	_ -> ewok_deployment_srv:undeploy(App)
	end.

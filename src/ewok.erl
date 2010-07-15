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

%% @doc Ewok AS: The 'Enterprise Wok'.
%% @author Steve Davis <steve@simulacity.com>
%% @copyright 2008-2010 Steve Davis. All Rights Reserved.
-module(ewok).

-include("ewok.hrl").
-include("ewok_system.hrl").

%% API
-export([start/0, start/1, start/2, stop/0]).
-export([ident/0, password/1, keystore/0, keystore/2, info/0, info/1, api/1, status/1]).
-export([config/0, config/1, config/2]).
-export([deploy/1, undeploy/1]).

%%
%% API
start() ->
	application:start(ewok).
%%
stop() -> 
	application:stop(ewok).

%% NOTE: consider an "embedded" mode startup?
start(App) when is_atom(App) ->
	not_implemented.
%% Placeholders
start(http, _Port) -> ok;
start(smtp, _Port) -> ok;
start(pop3, _Port) -> ok;
start(imap, _Port) -> ok;
start(amqp, _Port) -> ok;
start(ldap, _Port) -> ok;
start(dns,  _Port) -> ok;
start(_, _) -> error.

%%
ident() ->
	list_to_binary([
		?SERVER_ID, 
		<<" build#">>, ewok_util:build_number(), 
		<<" [">>, ewok_util:build_time(), <<"]">>
	]).

%% remove...
password(P) when is_list(P) ->
	password(list_to_binary(P));
password(P) when is_binary(P) ->
	ewok_identity:password(P).

%%
keystore() ->
	case is_pid(whereis(ewok_identity_srv)) of
	true ->
		ewok_identity:keystore();
	false ->
		not_running
	end.
%%
keystore(Key, Value) when is_atom(Key), is_binary(Value) ->
	case is_pid(whereis(ewok_identity_srv)) of
	true ->
		ewok_identity:keystore(Key, Value);
	false ->
		not_running
	end.

%%
config() ->
	Config = ewok_config:all(),
	io:format(user, "~p~n", [lists:sort(Config)]),
	{ok, length(Config)}.
	
config(Key) ->
	ewok_config:get_value(Key).
	
config(Key, Default) ->
	ewok_config:get_value(Key, Default).

%%
deploy(App)->
	ewok_deployment_srv:load(App),
	ewok_deployment_srv:deploy(App).
%%
undeploy(App) ->
	ewok_deployment_srv:undeploy(App).
	
%%
api(Module) ->
	case code:ensure_loaded(Module) of
	{'module', Module} ->
		Exports = Module:module_info(exports),
		Behaviours = [
			X:behaviour_info(callbacks) 
			|| {behaviour, [X]} 
			<- Module:module_info(attributes)],
		%?TTY({behaviour_funs, Behaviours}),
		Elide = [{module_info, 0}, {module_info, 1} | lists:flatten(Behaviours)],
		[X || X <- Exports, lists:member(X, Elide) =:= false];
	_ -> undefined
	end.
	
status(Module) when is_atom(Module) ->
	io:format("~p~n", [sys:get_status(Module)]).
	
%% reminder
info() ->
	[version, running, ports, config, routes, webapps, 
		users, auth, roles, sessions, tasks, {'module'}].
% TODO: This will likely be radically revised over the course of development
info(Key) -> 
	case Key of
	version -> 
		[Version|_] = proplists:get_value('vsn', ?MODULE:module_info(attributes)),
		{Y, Mo, D, H, M, S} = proplists:get_value(time, ?MODULE:module_info('compile')),
		Build = calendar:datetime_to_gregorian_seconds({{Y, Mo, D}, {H, M, S}}),
		{Version, Build};
	running -> whereis(ewok_sup) =/= undefined;
	ports -> inet:i();
	config -> ewok_config:print();
	routes -> ewok_config:print(ewok_route);
	webapps -> 
		case whereis(ewok_deployment_srv) of
		undefined -> {error, not_started};
		Pid when is_pid(Pid) ->
			{_,_,_,Apps} = ewok_deployment_srv:list(),
			Apps
		end;
	users -> ewok_db:select(ewok_user);
	auth -> ewok_db:select(ewok_auth);
	roles -> ewok_db:select(ewok_role);
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
			Key:module_info(attributes);
		_ -> undefined
		end
	end.


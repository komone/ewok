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
-include("ewok.hrl").
-include("ewok_system.hrl").

%% API
-export([start/0, start/2, stop/0]).
-export([ident/0, info/1]).
-export([config/0, config/1, config/2, mimetype/1]).
-export([deploy/1, undeploy/1]).

%% NOTE: consider an "embedded" mode startup?

%%
%% API
start() ->
	application:start(ewok).
%%
stop() -> 
	application:stop(ewok).

%%
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
	
%%
config() ->
	ewok_config:all().
config(Key) ->
	ewok_config:get_value(Key).
config(Key, Default) ->
	ewok_config:get_value(Key, Default).
mimetype(Ext) ->
	ewok_config:mimetype(Ext).

%%
deploy(App)->
	ewok_deployment_srv:load(App),
	ewok_deployment_srv:deploy(App).
%%
undeploy(App) ->
	ewok_deployment_srv:undeploy(App).
	
% TOO: This will likely be radically revised over the course of development
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


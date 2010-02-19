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

-module(ewok_deployment_srv).
-name("Ewok Deployment Service").

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-export([validate/2, load/1, unload/1, deploy/1, undeploy/1]).
-export([list/0, package/1]).

-compile(export_all).

-define(SERVER, ?MODULE).

%%
start_link(Args) ->
	Runmode = ewok_config:get_value({ewok, runmode}, development),
	AppRoot = ewok_config:get_value({ewok, http, deploy_root}, ?APP_ROOT),
	ewok_log:message(?MODULE, {configuration, [{runmode, Runmode}, {deploy_root, AppRoot}]}),
    
	gen_server:start_link({local, ?SERVER}, ?MODULE, [Runmode, AppRoot | Args], []).
%%
stop() ->
    gen_server:cast(?SERVER, stop).
%%
load(App = #web_app{}) ->
	gen_server:call(?SERVER, {load, App}, infinity);
load(App) when is_atom(App) ->
	gen_server:call(?SERVER, {load, App}, infinity).
%%
unload(App) when is_atom(App) ->
	gen_server:call(?SERVER, {unload, App}, infinity).
%%
deploy(App) when is_atom(App) ->
	gen_server:call(?SERVER, {deploy, App}, infinity).
%%
undeploy(App) when is_atom(App) ->
	gen_server:call(?SERVER, {undeploy, App}, infinity).
%%
list() ->
	gen_server:call(?SERVER, {list}, infinity).

%% TODO: placeholder
%% .ezw is a web archive containing a .web config 
%% .ezs would be a service archive (test with egeoip?) containing a .svc config
package(App) when is_atom(App) ->
	Dir = ewok_util:appdir(App),
	File = filename:basename(Dir), 
	%% AppName = atom_to_list(App),
	zip:create(File ++ ".ez", [File], [
		{cwd, filename:dirname(Dir)}, 
		{compress, all}, 
		{uncompress,[".beam",".app"]}
	]).

%%
%% gen_server callbacks 
%%

%%
init([Runmode, AppRoot]) ->
	DeployDir = ewok_file:path([ewok_util:appdir(), AppRoot]),
	AppList = ewok_file:find(DeployDir, <<"\\.app$|\\.ez$">>, recursive),
	Apps = load_paths(AppList, []),
    {ok, #server{runmode=Runmode, path=DeployDir, apps=Apps}}.
%%
handle_call({load, WebApp = #web_app{id = App}}, _From, State) -> 
	NewState = State#server{apps = lists:keystore(App, 2, State#server.apps, WebApp)},
	{reply, ok, NewState};
%%
handle_call({load, App}, _From, State) -> 
	{Reply, NewState} = 
		case load_app(App) of
		WebApp = #web_app{} ->
			{ok, State#server{apps = lists:keystore(App, 2, State#server.apps, WebApp)}};
		Error ->
			{Error, State}
		end,
	{reply, Reply, NewState};
%%
handle_call({unload, _App}, _From, State) -> 
	Reply = not_implemented,
    {reply, Reply, State};
%%
handle_call({deploy, App}, _From, State) ->
	{Reply, NewState} =
		case lists:keyfind(App, 2, State#server.apps) of
		WebApp = #web_app{} when WebApp#web_app.valid =:= true ->
			case WebApp#web_app.config of
			undefined ->
				{{error, {no_config, App}}, State};
			Config ->
				ewok_config:load(App, http, Config),
				UpdatedApps = lists:keyreplace(App, 2, State#server.apps, WebApp#web_app{deployed = true}),
				{ok, State#server{apps = UpdatedApps}}
			end;
		false ->
			{{not_loaded, App}, State}
		end,
	{reply, Reply, NewState};
%% 
handle_call({undeploy, App}, _From, State) -> 
	{Reply, NewState} = 
		case lists:keyfind(App, 2, State#server.apps) of
		false -> {ok, State};
		WebApp = #web_app{} ->
			Result = ewok_config:unload(App),
			AppList = lists:keyreplace(WebApp#web_app.id, 2, 
				State#server.apps, WebApp#web_app{deployed=false}),
			{Result, State#server{apps=AppList}}
		end,
    {reply, Reply, NewState};
%% 
handle_call({list}, _From, State) -> 
    {reply, State, State}.
%%
handle_cast(stop, State) ->
    {stop, normal, State};
%%
handle_cast(_Msg, State) ->
    {noreply, State}.
%%
handle_info(_Msg, State) ->
    {noreply, State}.
%%
terminate(_Reason, _State) ->
    ok.
%%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.	

%% section: internal
load_paths([Path|Rest], Acc) ->
	case load_path(Path) of
	App = #web_app{} ->
		case lists:keyfind(App#web_app.id, 2, Acc) of
		false ->
			ewok_log:message(?MODULE, {loaded, App}),
			load_paths(Rest, [App | Acc]);
		_ ->
			ewok_log:message(?MODULE, {duplicate, App}),
			%?TTY({removing, App}), 
			code:del_path(App#web_app.id), %% TODO: never load
			load_paths(Rest, Acc)
		end;
	Error ->
		ewok_log:message(?MODULE, {Error, Path}),
		load_paths(Rest, Acc)
	end;
%%
load_paths([], Acc) ->
	lists:reverse(Acc).

%% full_path() -> #web_app{} | undefined
load_path(Path) ->
	case ewok_file:code_path(Path) of 
	undefined ->
		{error, invalid_path};
	BeamDir -> 
		App = ewok_file:appname(BeamDir),
		code:add_patha(binary_to_list(BeamDir)),
		load_app(App)
	end.

%%
load_app(App) ->
	application:unload(App),
	ok = application:load(App),
	case application:get_env(App, web_app) of
	{ok, Config} ->
%		?TTY({loading, App, Config}),
		case validate(App, Config) of
		ok ->
			#web_app{id = App, path = code:lib_dir(App), config = Config, valid = true, deployed = false};
		Error ->
			Error
		end;
	Error -> 
		Error
	end.

%%
validate(_App, Config) ->
	try begin
		ok = validate_routes(Config)
	end catch 
	Error:Reason ->
		{Error, Reason}
	end.	

%%
validate_routes([H = #route{}|T]) ->
	case check_behaviour(H#route.handler, ewok_http_resource) of
	ok ->
		ok = check_persistence(H#route.handler),
		case ewok_db:lookup(ewok_route, list_to_binary(H#route.path)) of
		undefined -> 
			validate_routes(T);
		#ewok_route{} -> 
			?TTY({duplicate_route, H}),
			% TODO: When apps can be undeployed, this should be uncommented
			%{error, {duplicate_route, H}}
			validate_routes(T)
		end;
	Error -> Error
	end;
validate_routes([_H|T]) ->
	validate_routes(T);
validate_routes([]) ->
	ok.
	
%%
check_behaviour(Module, Behaviour) ->
	case code:ensure_loaded(Module) of 
	{'module', Module} ->
		Attrs = Module:module_info(attributes),
		Behaviours = lists:append([
			proplists:get_value('behaviour', Attrs, []),
			proplists:get_value('behavior', Attrs, [])
		]),
		case [X || X <- Behaviours, X =:= Behaviour] of
		[Behaviour|_] -> ok;
		[] -> {error, {'behaviour', Behaviour}}
		end;
	Error -> Error
	end.

%%
check_persistence(App) ->
	case proplists:get_value(ewok_db, App:resource_info()) of
	undefined ->
		ok;
	Tables ->
		{ok, Created} = ewok_db:create_missing(Tables),
		case Created of
		[] ->
			ok;
		_ ->
			?TTY({created, Created}),
			ewok_log:info({created, Created}),
			ok
		end
	end.

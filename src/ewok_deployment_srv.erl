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

-module(ewok_deployment_srv).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("../include/ewok.hrl").
-include("ewok_system.hrl").

-behavior(ewok_service).
-export([start_link/0, stop/0, service_info/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-export([validate/1, load/1, unload/1, deploy/1, undeploy/1]).
-export([list/0, package/1]).

-define(SERVER, ?MODULE).

start_link() ->
	%% dependency check
	true = is_pid(whereis(ewok_cache_srv)),
	
	Runmode = ewok:config({ewok, runmode}),
	AppRoot = ewok:config({ewok, http, deploy_root}, ?DEFAULT_APP_ROOT),
	ewok_log:log(default, service, {?MODULE, service_info()}),
	ewok_log:log(default, configuration, {?MODULE, [{runmode, Runmode}, {deploy_root, AppRoot}]}),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Runmode, AppRoot], []).

stop() ->
    gen_server:cast(?SERVER, stop).

service_info() -> [
	{name, "Ewok Deployment Service"},
	{version, ?VERSION},
	{comment, ""},
	{depends, [ewok_cache_srv, ewok_scheduler_srv]}
].

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
		{uncompress,[".beam",".app",".web"]}
	]).

%%
load(Path) when is_list(Path) ->
	not_implemented;
%
load(App) when is_atom(App) ->
	gen_server:call(?SERVER, {load, App}, infinity).
%
unload(App) when is_atom(App) ->
	gen_server:call(?SERVER, {unload, App}, infinity).

%
deploy(Path) when is_list(Path) ->
	not_implemented;
%
deploy(App) when is_atom(App) ->
	gen_server:call(?SERVER, {deploy, App}, infinity).
	
%%
undeploy(App) when is_atom(App) ->
	gen_server:call(?SERVER, {undeploy, App}, infinity).

%
list() ->
	gen_server:call(?SERVER, {list}, infinity).


%%
%% gen_server callbacks 
%%
init([Runmode, AppRoot]) ->
	DeployDir = filename:join(code:lib_dir(ewok), AppRoot),
	{ok, Files} = file:list_dir(DeployDir),
	Paths = [filename:join(DeployDir, X) || X <- Files],
	AppList = load_paths(Paths, []),
    {ok, #server{runmode=Runmode, dir=DeployDir, apps=AppList}}.

%%
handle_call({load, App}, _From, State) -> 
	Reply = App,
    {reply, Reply, State};
%%
handle_call({unload, App}, _From, State) -> 
	Reply = App,
    {reply, Reply, State};
%%
handle_call({deploy, App}, _From, State) ->
	{Reply, NewState} = 
		case ewok_configuration:load(App) of
		{ok, _} ->
			%% More TODO!!
			case erlang:function_exported(App, init, 1) of
			true -> App:init([]);
			false -> ok
			end,
			case lists:keyfind(App, 2, State#server.apps) of
			false -> 
				{{error, app_not_found}, State}; %% CHANGE!!!
			WebApp = #web_app{} ->
				WebApp1 = WebApp#web_app{deployed = true},
				UpdatedApps = lists:keyreplace(App, 2, State#server.apps, WebApp1),
				UpdatedState = State#server{apps = UpdatedApps},
				{ok, UpdatedState}
			end
		end,
	{reply, Reply, NewState};
%% 
handle_call({undeploy, App}, _From, State) -> 
	{Reply, NewState} = 
		case lists:keyfind(App, 2, State#server.apps) of
		false -> {ok, State};
		WebApp = #web_app{} ->			
			Result = ewok_configuration:unload(App),
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
%
handle_cast(_Msg, State) ->
    {noreply, State}.
%%
handle_info(_Msg, State) ->
    {noreply, State}.
%%
terminate(_Reason, _State) ->
    ok.
%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.	

%%
%% Internal
%%
load_paths([H|T], Acc) ->
	case load_path(H) of
	App = #web_app{} ->
		load_paths(T, [App | Acc]);
	undefined -> % log this
		load_paths(T, Acc)
	end;
load_paths([], Acc) ->
	lists:reverse(Acc).

%% full_path() -> #web_app{} | undefined
load_path(Path) when ?is_string(Path) ->
	case filelib:is_dir(Path) of 
	true -> 
		load_path1(Path);
	false ->
		case filelib:is_regular(Path) 
			andalso filename:extension(Path) =:= ?ARCHIVE_FILE_EXT of
		true ->
			load_path1(filename:join([Path, filename:basename(Path, ".ez")]));
		false -> 
			undefined
		end
	end.
%%
load_path1(Path) ->
	BeamDir = filename:join(Path, "ebin"),
	%?TTY("Trying: ~p~n", [Path]),
	case filelib:wildcard("*" ++ ?CONFIG_FILE_EXT, BeamDir) of
	[AppFile|_] ->
		%?TTY("AppFile: ~p~n", [{AppFile, BeamDir}]),
		AppName = list_to_atom(filename:basename(AppFile, ?CONFIG_FILE_EXT)),
		code:add_pathz(BeamDir),
		Valid = validate(AppName),
		#web_app{id=AppName, path=Path, valid=Valid};
	_ -> 
		undefined
	end.


%%
validate(App) when is_atom(App) ->
	case check_behaviour(App, ewok_web_application) of
	ok ->
		case ewok_configuration:preload(App) of
		{ok, _File, Config} -> 
			Routes = [X || X = #route{} <- Config],
			validate_routes(Routes);
		Error -> Error
		end;
	Error -> Error
	end.
	
%%
validate_routes([H = #route{}|T]) ->
	case check_behaviour(H#route.handler, ewok_http_resource) of
	ok ->
		case ewok_cache:lookup(route, H#route.path) of
		undefined -> validate_routes(T);
		#route{} -> {error, {duplicate_route, H}}
		end;
	Error -> Error
	end;
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
	Value -> {error, Value}
	end.

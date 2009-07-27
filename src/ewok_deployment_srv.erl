%%
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

-export([install/1]).
-export([deploy/1, undeploy/1]).
-export([make_ez/1]).
-compile(export_all).

-define(SERVER, ?MODULE).

start_link() ->
	Runmode = ewok_config:get("ewok.runmode"),
	AppRoot = ewok_config:get("ewok.http.app_root", "./priv/apps"),
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
make_ez(Appname) ->
	 zip:create(Appname ++ "-1.0.0.ezw", [Appname], 
		[{compress, all}, {uncompress,[".beam",".app",".web"]}]).
		
%	
install(true) ->
	verify_install();
	% ..more...
install(_) ->
	verify_install().

%
info() ->
	gen_server:call(?SERVER, {info}, infinity).
%
deploy(Path) when is_list(Path) ->
	not_implemented;
%
deploy(App) when is_atom(App) ->
	gen_server:call(?SERVER, {deploy, App}, infinity).
	
%%
undeploy(App) when is_atom(App) ->
	gen_server:call(?SERVER, {undeploy, App}, infinity).

%%
%% gen_server callbacks
%%
init([Runmode, AppRoot]) ->
	DeployDir = filename:join(ewok_util:appdir(), AppRoot),
	AppFiles = filelib:wildcard("*/ebin/*" ++ ?CONFIG_FILE_EXT, DeployDir),
	AppDirs = [filename:dirname(filename:join(DeployDir, F))|| F <- AppFiles],
	AppNames = [list_to_atom(filename:basename(F, ?CONFIG_FILE_EXT))|| F <- AppFiles],
	code:add_paths(AppDirs),
	Validation = [validate_web_app(A) || A <- AppNames],
	F = fun(Name, Dir, Valid) -> #web_app{id=Name, path=Dir, valid=Valid} end,
	Apps = lists:zipwith3(F, AppNames, AppDirs, Validation),
	%%
	ewok_cache:add(Apps), %% maybe use this instead of state...
	%?TTY("{~p:~n~p}~n", [?MODULE, Apps]),
    {ok, #server{runmode=Runmode, dir=DeployDir, apps=Apps}}.
%%
handle_call({deploy, App}, _From, State) ->
	{Reply, NewState} = 
		case code:which(App) of
		non_existing -> 
			{{error, not_found, App}, State};
		_ -> 
			case ewok_config:load(App) of
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
				end;
			Error -> {Error, State}
			end
		end,
	{reply, Reply, NewState};
%% 
handle_call({undeploy, _App}, _From, State) -> 
	Reply = ok,
    {reply, Reply, State};
%% 
handle_call({info}, _From, State) -> 
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

%% Internal 

%
verify_install() ->
	try begin
		[Node] = mnesia:system_info(db_nodes),
		true = Node =:= node() andalso node() =/= 'nonode@nohost',
		true = filelib:is_dir(mnesia:system_info(directory)),
		true = mnesia:system_info(use_dir),
		ok
	end catch
		_:_ -> {error, get_install_props()}
	end.

%%
get_install_props() ->
	Node = node(),
	IsRemote = (Node =/= 'nonode@nohost'),
	UsesDb = ([Node] =:= [N || DbNode = N <- mnesia:system_info(db_nodes), DbNode =:= Node]),
	HasDir = mnesia:system_info(directory),
	UsesDir = mnesia:system_info(use_dir),
	[{node, Node}, {remote, IsRemote}, {db_node, UsesDb}, {db_dir, HasDir}, {db_used, UsesDir}].
	
%
validate_web_app(_A) -> 
	true.
%
deploy_web_app(_A) ->
	ok.

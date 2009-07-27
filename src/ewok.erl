%%
%%
-module(ewok).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl").

%% API
-export([start/0, stop/0]).
-export([id/0]).
-export([info/1, config/0, configure/0]).
-export([deploy/1, deploy/2, undeploy/1, undeploy/2]).

-define(BOOT_LOG_ID, boot).

%%
id() ->
	list_to_binary([
		?SERVER_ID, 
		<<" build#">>, ewok_util:build_number(), 
		<<" [">>, ewok_util:build_time(), <<"]">>
	]).
%%
%% API
%%
start() ->
	%% Load early to get access to the ewok application env
	application:load(ewok),
	
	{ok, Opts} = application:get_env(ewok, boot),
	LogId = proplists:get_value(log, Opts, ?BOOT_LOG_ID),
	ewok_logging_srv:start_link(LogId), %% ensure boot logging is available

	%% IMPL: note that at this time, the boot log is the default log
	ewok_log:log(default, server, id()),
	ewok_log:log(default, loaded, application:loaded_applications()),
	
	%{ok, AutoInstall} = application:get_env(ewok, autoinstall),
	case application:start(ewok) of
	ok ->
		Deploy = ewok_config:get({ewok, autodeploy}, []),
		Results = [ewok_deployment_srv:deploy(App) || App <- Deploy],
		case lists:dropwhile(fun(X) -> X =:= ok end, Results) of
		[] -> 
			ewok_log:add_log(server),
			ewok_log:set_default(server),
			ewok_log:remove_log(?BOOT_LOG_ID),
			ok;
		Errors -> {error, Errors}
		end;
	Error -> Error 
	end.
	
%%
stop() -> 
	case application:stop(ewok) of
	ok -> ewok_log:stop(); %% still needed?
	Error -> Error
	end.

% TOO: This will likely be radically revised over the course of development
info(Key) -> 
	case Key of
	version -> 
		[{Major, Minor, Patch}|_] = proplists:get_value('vsn', ?MODULE:module_info(attributes)),
		{Y, Mo, D, H, M, S} = proplists:get_value(time, ?MODULE:module_info('compile')),
		Build = calendar:datetime_to_gregorian_seconds({{Y, Mo, D}, {H, M, S}}),
		{Major, Minor, Patch, Build};
	running -> whereis(ewok_sup) =/= undefined;
	config -> ewok_config:print();
	routes -> ewok_config:print(route);
	webapps -> ewok_config:print(web_app);
	users -> ewok_db:select(user);
	auth -> ewok_db:select(auth);
	roles -> ewok_db:select(role);
	sessions ->	 
		case whereis(ewok_session_srv) of 	
		undefined -> undefined;
		_ -> 
			Sessions = ewok_session_srv:sessions(),
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

% 
config() ->
	configure().
configure() ->
	ewok_config:load().

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

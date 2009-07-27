%% 
-module(ewok_sup).
-vsn("1.0").
-author('steve@simulacity.com').

-behaviour(application).
-export([start/2, config_change/3, prep_stop/1, stop/1]).

-behaviour(supervisor).
-export([init/1]).

%% remove "upgrade" or figure out what this is really for (from mochiweb)
-export([service_info/0, upgrade/0]).

%%
service_info() ->
	[ {name, "Ewok AS Supervisor"}, {version, {1,0,0}}, {depends, []} ].

%%
%% application Callbacks 
%% 
start(_Type, Args) -> 
	crypto:start(),%% TEMPORARILY...	
	%% check that boot logging is turned on!
	case whereis(ewok_logging_srv) of
	Pid when is_pid(Pid) -> supervisor:start_link({local, ?MODULE}, ?MODULE, Args);
	_ -> io:format(user, "Please use ewok:start() rather than application:start(ewok).", [])
	end.
%
config_change(_Changed, _New, _Removed) ->  
	ok.
%
prep_stop(State) -> 
	State.
%% 
stop(_State) -> 
	ok.

%%
%% supervisor callback
%%
init(Args) ->
    {ok, _IP} = %% Maybe use later -- for now ignore
		case os:getenv("EWOK_IP") of 
		false -> {ok, {0,0,0,0}};
		Any -> inet_parse:address(Any) 
		end,	
	ChildSpecs = [spec(Service) || Service <- Args],
    {ok, {{one_for_one, 10, 10}, ChildSpecs}}.

%% TODO: Review all of these options on a per-service basis after
%% performance tests. For now, just set reasonable defaults.
spec(cache) -> 
	{ewok_cache_srv, {ewok_cache_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_cache_srv]};
spec(datasource) -> 
	{ewok_data_srv, {ewok_data_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_data_srv]};
spec(identity) -> 
	{ewok_identity_srv, {ewok_identity_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_identity_srv]};
spec(scheduler) -> 
	{ewok_scheduler_srv, {ewok_scheduler_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_scheduler_srv]};
spec(deployer) -> 
	{ewok_deployment_srv, {ewok_deployment_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_deployment_srv]};
spec(session) -> 
	{ewok_session_srv, {ewok_session_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_session_srv]};
spec(workflow) ->
	{esp_workflow_sup, {esp_workflow_sup, start_link, []}, 
		permanent, infinity, supervisor, [esp_workflow_sup]};
spec(http) -> 
	{ewok_http_srv, {ewok_http_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_http_srv]};
spec(geoip) -> 
	{ewok_geoip_srv, {ewok_geiop_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_geiop_srv]};
spec(Other) when is_atom(Other) ->
	% code:ensure_loaded(Other), erlang:function_exported(start_link, 0)..,?
	{Other, {Other, start_link, []}, 
		permanent, 5000, worker, [Other]}.

%% NOT USED (yet?)
%% from mochiweb
upgrade() ->
    {ok, {_, Specs}} = init([]),
    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

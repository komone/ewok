-module(esp_workflow_sup).
-vsn({1,0,0}).

-behaviour(supervisor).
-export([init/1]).

-behavior(ewok_service).
-export([start_link/0, stop/0, service_info/0]).

%%
start_link() ->
	ewok_log:log(default, service, {?MODULE, service_info()}),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
	%% supervisor:terminate_child... all?
	ok.

service_info() -> [{name, "ESP Workflow Engine"}].

%%
init([]) -> 
	{ok, {{one_for_one, 10, 10}, []}}.
	

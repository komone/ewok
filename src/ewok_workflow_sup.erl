-module(ewok_workflow_sup).
-vsn({1,0,0}).

-include("ewok_system.hrl").

-behaviour(supervisor).
-export([init/1]).

-behavior(ewok_service).
-export([start_link/0, stop/0, service_info/0]).

-export([create/1]).

%%
start_link() ->
	ewok_log:log(default, service, {?MODULE, service_info()}),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
	%% supervisor:terminate_child... all?
	ok.

service_info() -> [{name, "Ewok Workflow Engine"}].

%%
init([]) -> 
	{ok, {{one_for_one, 10, 10}, []}}.
	
%%
create(Workflow) ->
	ChildSpec = {Workflow#workflow.id, 
		{ewok_workflow, start_link, [Workflow]}, 
		transient, 5000, worker, [ewok_workflow]},
	supervisor:start_child(?MODULE, ChildSpec).

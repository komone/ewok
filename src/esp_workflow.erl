%% TODO: Not implemented/used...
%% ...a possible replacement for request/session pseudo-objects
%%
-module(esp_workflow).

-behavior(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4,
	handle_info/3, terminate/3, code_change/4]).

-export([create/1]).

-record(sdata, {workflow}).
-record(workflow, {id, name, steps=[]}).

%%
create(Workflow) ->
	ChildSpec = {Workflow#workflow.id, 
		{esp_workflow, start_link, [Workflow]}, 
		transient, 5000, worker, [esp_workflow]},
	supervisor:start_child(esp_workflow_sup, ChildSpec).

%%
init([Workflow]) ->
	{ok, new_request, #sdata{workflow=Workflow}}.

%%
handle_event(_Event, State, StateData) ->
	%
	{next_state, State, StateData}.
%%
handle_sync_event(_Event, _From, State, StateData) ->
	%
	{next_state, State, StateData}.
%%
handle_info(_Info, State, StateData) ->
	%
	{next_state, State, StateData}.
%%
terminate(_Reason, _State, _StateData) ->
	 void.
%%
code_change(_OldVsn, State, StateData, _Extra) ->
	{ok, State, StateData}.

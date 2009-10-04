%% TODO: Not implemented/used...
%% ...a possible replacement for request/session pseudo-objects
%%
-module(ewok_workflow).

-include("ewok_system.hrl").

-behavior(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4,
	handle_info/3, terminate/3, code_change/4]).


-record(sdata, {workflow}).

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

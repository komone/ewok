%% TODO: Not implemented/used...
%% ...a possible replacement for request/session pseudo-objects
%%
-module(ewok_workflow).

-include("ewok_system.hrl").

-behavior(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4,
	handle_info/3, terminate/3, code_change/4]).


-record(state, {workflow}).

%%
init([Workflow]) ->
	{ok, new_request, #state{workflow=Workflow}}.
%%
handle_event(_Event, StateName, State) ->
	%
	{next_state, StateName, State}.
%%
handle_sync_event(_Event, _From, StateName, State) ->
	%
	{next_state, StateName, State}.
%%
handle_info(_Info, StateName, State) ->
	%
	{next_state, StateName, State}.
%%
code_change(_OldVsn, StateName, State, _Extra) ->
	{ok, StateName, State}.
%%
terminate(_Reason, _StateName, _State) ->
	 void.

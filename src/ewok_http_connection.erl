%% TODO: Not implemented/used...
%% ...a possible replacement for request/session pseudo-objects
%%
-module(ewok_http_connection).

-behavior(gen_fsm).
-export([init/1, handle_event/3, handle_sync_event/4,
	handle_info/3, terminate/3, code_change/4]).
-export([new_request/2]).

-record(sdata, {socket}).

%%
init([Socket]) ->
	{ok, new_request, [#sdata{socket=Socket}]}.

%%
new_request(_Event, Data) ->
	{next_state, request_ready, Data}.

%%
handle_event(_Event, State, Data) ->
	%
	{next_state, State, Data}.
%%
handle_sync_event(_Event, _From, State, Data) ->
	%
	{next_state, State, Data}.
%%
handle_info(_Info, State, Data) ->
	%
	{next_state, State, Data}.
%%
terminate(_Reason, _State, _Data) ->
	 void.
%%
code_change(_OldVsn, State, Data, _Extra) ->
	{ok, State, Data}.

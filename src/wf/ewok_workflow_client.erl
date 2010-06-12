-module(ewok_workflow_client).

-include("ewok.hrl").

-export([start/2, stop/1, get_work/2]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).


-define(TIMEOUT, 10000). % 10 seconds
-record(workitem, {id, roles, payload}). 

%%
get_work(Name, Roles) when is_atom(Name) ->
	gen_server:call(Name, {next, Roles}, ?TIMEOUT).

%%
start(Name, Args) when is_atom(Name) ->
	ewok_identity_srv:start_link([]),
	gen_server:start_link({local, Name}, ?MODULE, Args, []).
%%
stop(Name) ->
    gen_server:cast(Name, stop).
%%
init(Args) ->
	{ok, Args}.
%%
handle_call({next, Roles}, _From, State) ->
	Value = #workitem{
		id = ewok_identity:id(),
		roles = Roles,
		payload = [ewok_identity:uuid()]
	},
	{reply, Value, State}.
%%
handle_cast(stop, State) ->
    {stop, normal, State};
%
handle_cast(Message, State) ->
	?TTY({?MODULE, handle_info, Message}),
    {noreply, State}.
%%
handle_info(Message, State) ->
	?TTY({?MODULE, handle_info, Message}),
    {noreply, State}.
%%
terminate(_Reason, _State) ->
    ok.
%%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.

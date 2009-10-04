%%
-module(ewok_cache_srv).
-vsn("1.0.0").
-author('steve@simulacity.com').

-include("../include/ewok.hrl").
-define(SERVER, ?MODULE).

-behaviour(ewok_service).
-export([start_link/0, stop/0, service_info/0]).

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

%%
%% ewok_service callbacks
%%
start_link() -> 
	ewok_log:log(default, service, [{?MODULE, service_info()}]),
	gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
%
stop() ->
    gen_server:cast(?SERVER, stop).
%	
service_info() -> [ 
	{name, "Ewok Cache Service"},
	{version, {1,0,0}},
	{depends, []}
].

%%
%%% gen_server
%%
init([]) ->
	% trap_exit --> do we need this? *why* do we need this?
    process_flag(trap_exit, true), 
    {ok, []}.
%%
handle_call({add, Record}, _From, State) ->
	Type = element(1, Record),
	NewState = 
		case lists:member(Type, State) of
		false -> 
			ets:new(Type, [set, named_table, protected, {keypos, 2}]),
			[Type|State];
		true -> 
			State
		end,
	ets:insert(Type, Record),
    {reply, ok, NewState};
%%
handle_call({remove, Record}, _From, State) ->
	Type = element(1, Record),
	Key = element(2, Record),
	%% IMPL: don't use delete_object as the record
	%% may have had runtime changes
	true = ets:delete(Type, Key),
    {reply, ok, State};
%
handle_call({clear, Type}, _From, State) ->
	case lists:member(Type, State) of
	true -> ets:delete_all_objects(Type);
	false -> ok
	end,
	{reply, ok, State};
%
handle_call({reset}, _From, State) ->
	[ets:delete(X) || X <- State],
	{reply, ok, []}.
%%
handle_cast(stop, State) ->
    {stop, normal, State};
%
handle_cast(_, State) ->
    {noreply, State}.
%%
handle_info(_Info, State) ->
    {noreply, State}.
%%
terminate(_Reason, _State) ->
    ok.
%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.	

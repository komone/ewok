%% Copyright 2009 Steve Davis <steve@simulacity.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ewok_cache_srv).
-vsn("1.0.0").
-author('steve@simulacity.com').

-behaviour(ewok_service).
-export([start_link/0, stop/0, service_info/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

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
    % process_flag(trap_exit, true), % do we need this? *why* do we need this?
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
	true = ets:insert(Type, Record),
    {reply, ok, NewState};
%%
handle_call({remove, Record}, _From, State) ->
	Type = element(1, Record),
	Key = element(2, Record),
	%% IMPL: don't use delete_object as the record may have had runtime changes
	true = ets:delete(Type, Key),
    {reply, ok, State};
%
handle_call({clear, all}, _From, State) ->
	[ets:delete(X) || X <- State],
	{reply, ok, []};
%
handle_call({clear, Type}, _From, State) ->
	case lists:member(Type, State) of
	true -> ets:delete_all_objects(Type);
	false -> ok
	end,
	{reply, ok, State}.
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

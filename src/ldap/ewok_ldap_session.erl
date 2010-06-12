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

-module(ewok_ldap_session).
-author('steve@simulacity.com').
-include("ldap.hrl").

-compile(export_all).

-behaviour(gen_fsm).
%% gen_fsm callbacks
-export([init/1, handle_info/3, handle_event/3, 
	handle_sync_event/4, terminate/3, code_change/4]).

%% api
-export([create/0, create/1, request/2, close/1]).

%% FSM States
-export([bind/3, session/3]).

%
-record(state, {id, user}).

%% API
create() ->
	create([]).
create(Options) ->
    gen_fsm:start_link(?MODULE, [], Options).
%%
request(Pid, Request) when is_tuple(Request) ->
	gen_fsm:sync_send_event(Pid, Request);
request(Pid, close) ->
	close(Pid).
%%
close(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

%% callbacks: gen_fsm
init(Options) ->
    process_flag(trap_exit, true), %% should we trap exits?
	_Timeout = proplists:get_value(timeout, Options, 180),
	{ok, bind, #state{}}.

%%
%% state callbacks
%%
bind(_R = #bind_request{}, _From, State) ->
	{reply, service_ready, session, State};
bind(timeout, _From, State) ->
	{reply, protocol_error, quit, State};
bind(_Message, _From, State) ->
	{reply, protocol_error, quit, State}. 

%%
session(_R = #search_request{}, _From, State) ->
	{reply, success, session, State};
session(#unbind_request{}, _From, State) ->
	{reply, success, bind, State};
session(_Message, _From, State) ->
	{reply, unwilling_to_perform, session, State}.
	
%%
handle_event(stop, _StateName, State) ->
	ewok_log:message(ldap, stop),
	{stop, normal, State};
handle_event(Event, StateName, State) ->
	ewok_log:message(ldap, event, Event),
    {stop, {StateName, undefined_event, Event}, State}.
%
handle_sync_event(Event, _From, StateName, State) ->
	ewok_log:message(ldap, error, Event),
    {stop, {StateName, undefined_event, Event}, State}.
%
handle_info(Info, StateName, State) ->
	ewok_log:message(ldap, info, Info),
    {noreply, StateName, State}.
%
code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.
%
terminate(_Reason, _StateName, _State) ->
    ok.

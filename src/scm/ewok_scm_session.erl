%% Copyright 2009-2010 Steve Davis <steve@simulacity.com>
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

-module(ewok_scm_session).
-author('steve@simulacity.com').

-compile(export_all).

-behaviour(gen_fsm).
%% gen_fsm callbacks
-export([init/1, handle_info/3, handle_event/3, 
	handle_sync_event/4, terminate/3, code_change/4]).

%% api
-export([create/0, create/1, request/2, close/1]).

%% FSM States
-export([data/3]).

-define(LOG_ID, git).

%% Parameterize
-define(GIT, "\"C:/Program Files/Git/bin/git.exe\"").
-define(REPO_PATH, "D:/Erlang/lib/").

%
-record(state, {id, cmd, from, to=[], body=[]}).

%% API
create() ->
	create([]).
create(Options) ->
    gen_fsm:start_link(?MODULE, [], Options).
	
%%
request(Pid, {Command, Args}) ->
	gen_fsm:sync_send_event(Pid, {Command, Args});
request(Pid, close) ->
	close(Pid).
%%
close(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

%%
%% state callbacks
%%
data(Message, _From, State) ->
	io:format("~p~n", [Message]),
	{reply, close, data, State}.

%% callbacks: gen_fsm
init(Options) ->
    process_flag(trap_exit, true), %% should we trap exits?	
	_Timeout = proplists:get_value(timeout, Options, 180),	
	RepoDir = proplists:get_value(path, Options, ?REPO_PATH),
	case filelib:is_dir(RepoDir) of
	true ->
		Command = ?GIT ++ " upload-pack " ++ RepoDir,
		{ok, data, #state{cmd = Command}};
	false ->
		{error, no_port}
	end.
	
%% TODO: complete callback set
handle_event(stop, _StateName, State) ->
	ewok_log:message(?LOG_ID, stop),
	{stop, normal, State};
handle_event(Event, StateName, State) ->
	ewok_log:message(?LOG_ID, event, Event),
    {stop, {StateName, undefined_event, Event}, State}.

handle_sync_event(Event, _From, StateName, State) ->
	ewok_log:message(?LOG_ID, error, Event),
    {stop, {StateName, undefined_event, Event}, State}.

handle_info(Info, StateName, State) ->
	ewok_log:message(?LOG_ID, info, Info),
    {noreply, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.

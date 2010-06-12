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

-module(ewok_smtpd_session).
-author('steve@simulacity.com').
-include("email.hrl").

-compile(export_all).

-behaviour(gen_fsm).
%% gen_fsm callbacks
-export([init/1, handle_info/3, handle_event/3, 
	handle_sync_event/4, terminate/3, code_change/4]).

%% api
-export([create/0, create/1, request/2, close/1]).

%% FSM States
-export([connected/3, session/3, mail/3, data/3]).

%
-record(state, {id, from, to=[], body=[]}).

%% API
create() ->
	create([]).
create(Options) ->
    {ok, Pid} = gen_fsm:start_link(?MODULE, [], Options),
	{ok, Pid, service_ready}.

%%
request(Pid, {Command, Args}) ->
	gen_fsm:sync_send_event(Pid, {Command, Args});
request(Pid, close) ->
	close(Pid).
%%
close(Pid) ->
	gen_fsm:send_all_state_event(Pid, stop).

%% callbacks: gen_fsm
init(Options) ->
    process_flag(trap_exit, true), %% should we trap exits?	
	_Timeout = proplists:get_value(timeout, Options, 180),
	{ok, connected, #state{}}.

%%
%% state callbacks
%%
connected({'EHLO', _}, _From, State) ->
	{reply, service_ready, session, State};
connected({'HELO', _}, _From, State) ->
	{reply, service_ready, session, State};
connected(timeout, _From, State) ->
	{reply, transaction_failed, quit, State};
connected(Message, _From, State) ->
	{reply, do_command(Message), connected, State}. 

%%
session({'MAIL', MailFrom}, _From, State) ->
	{reply, service_ready, mail, State#state{from=MailFrom, to=[]}};
session(Message, _From, State) ->
	{reply, do_command(Message), session, State}. 
	
%%
mail({'RCPT', MailTo}, _From, State) ->
	NewState = State#state{to=[MailTo|State#state.to]},
	{reply, service_ready, mail, NewState};
mail({'DATA', []}, _From, State) ->
	{reply, start_mail_input, data, State};
mail(Message, _From, State) ->
	{reply, do_command(Message), mail, State}. 

%%
data({_, <<$.>>}, _From, State) ->
	ewok_log:message(mail, State),
	ewok_smtp_store:save(State#state{id=ewok_identity:key()}),
	{reply, service_ready, session, #state{}};
data(Message, _From, State) ->
	NewState = State#state{body=[Message|State#state.body]},
	{reply, continue, data, NewState}.

%%
do_command({'HELP', _}) ->
	help_message;
do_command({'NOOP', _}) ->
	service_ready;
do_command({'RSET', _}) ->
	service_ready;
do_command({'QUIT', _}) ->
	closing_channel;
do_command(_) ->
	bad_command_sequence.
	
%% TODO: complete callback set
handle_event(stop, _StateName, State) ->
	ewok_log:message(mail, stop),
	{stop, normal, State};
handle_event(Event, StateName, State) ->
	ewok_log:message(mail, event, Event),
    {stop, {StateName, undefined_event, Event}, State}.

handle_sync_event(Event, _From, StateName, State) ->
	ewok_log:message(mail, error, Event),
    {stop, {StateName, undefined_event, Event}, State}.

handle_info(Info, StateName, State) ->
	ewok_log:message(mail, info, Info),
    {noreply, StateName, State}.

code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.

terminate(_Reason, _StateName, _State) ->
    ok.


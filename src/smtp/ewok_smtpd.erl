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

-module(ewok_smtpd).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("email.hrl").

-behaviour(ewok_inet).
-export([start/1, init/2, terminate/3]).
%% FSM States
-export([connected/2, session/2, mail/2, data/2]).
%% TEMP

%
-record(state, {id, ip, from, to, body = []}).

start(Port) ->
	#ewok_inet{
		port = Port,
		protocol = smtp,
		handler = ?MODULE,
		codec = ewok_smtp,
		timeout = 30
	}.
	
%% callbacks: ewok_inet
init(_Options, RemoteIp) ->
	{reply, service_ready, connected, #state{ip = RemoteIp}}.
%%
terminate(_Reason, _StateName, _State) ->
    ok.

%%
%% state callbacks
%%
connected({'EHLO', _}, State) ->
	{reply, ok_completed, session, State};
connected({'HELO', _}, State) ->
	{reply, ok_completed, session, State};
connected(Message, State) ->
	do_command(Message, connected, State). 

%%
session({'MAIL', MailFrom}, State) ->
	{reply, ok_completed, mail, State#state{from=MailFrom, to=[]}};
session(Message, State) ->
	do_command(Message, session, State). 
	
%%
mail({'RCPT', MailTo}, State = #state{to = Recipients}) ->
	NewState = State#state{to = lists:append(Recipients, MailTo)},
	{reply, ok_completed, mail, NewState};
mail({'DATA', []}, State) ->
	{reply, start_mail_input, data, State};
mail(Message, State) ->
	do_command(Message, mail, State). 
	
%%
data({data, Headers, Message}, State = #state{body = Body}) ->
	ewok_log:message(mail, State),
	case append(Message, Body) of
	{continue, NewBody} ->
		{noreply, data, State#state{body = NewBody}};
	{ok, NewBody} ->		
		Mail = #message{
			id = ewok_identity:key(),
			ip = State#state.ip,
			timestamp = ewok_util:timestamp(),
			from = State#state.from, 
			to = State#state.to,
			headers = Headers,
			body = NewBody
		},
		ok = ewok_smtp_store:save(Mail),
		{reply, ok_completed, session, #state{}}
	end.
%%
append([<<$.>>|_], Acc) ->
	{ok, lists:reverse(Acc)};
append([H|T], Acc) ->
	append(T, [H|Acc]);
append([], Acc) ->
	{continue, Acc}.

%%
do_command({'HELP', _}, State, StateData) ->
	{reply, help_message, State, StateData};
do_command({'NOOP', _}, State, StateData) ->
	{reply, ok_completed, State, StateData};
do_command({'RSET', _}, State, _) ->
	{reply, ok_completed, State, #state{}};
do_command({'QUIT', _}, _, _) ->
	{reply, closing_channel, terminate, #state{}};
do_command(_, State, StateData) ->
	{reply, bad_command_sequence, State, StateData}.


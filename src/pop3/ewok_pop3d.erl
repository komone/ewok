%% Copyright 2010 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(ewok_pop3d).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("email.hrl").

-compile(export_all).

-define(SOCKET_TIMEOUT_SECONDS, 600).

%% Consider what to do about this...
-define(DOMAIN, <<"ewok">>).

-behaviour(ewok_inet).
-export([start/1, init/2, terminate/3]).

%
-record(state, {id, ip, from, to=[], body=[]}).

start(Port) ->
	#ewok_inet{
		port = Port,
		protocol = pop3,
		handler = ?MODULE,
		codec = ewok_pop3,
		timeout = 10
	}.

%% callbacks: ewok_inet
init(_Options, {RemoteIp, _RemotePort}) ->
	{reply, {ok, <<"POP3 server ready">>}, authorization, #state{ip = RemoteIp}}.
%%
terminate(_Reason, _StateName, _State) ->
    ok.

%%
authorization({'USER', [Username]}, State) ->
	case ewok_users:exists(?DOMAIN, Username) of
	true ->
		{reply, {ok, <<"USER ", Username/binary>>}, authorization, State#state{id=Username}};
	false ->
		{reply, {error, <<"No such user">>}, authorization, State}
	end;
authorization({'PASS', Password}, State = #state{id=Username}) ->
	case ewok_users:login(?DOMAIN, Username, Password) of
	{ok, _User} ->
		{reply, {ok, <<"Maildrop locked and ready">>}, transaction, State};
	{error, _Reason} ->
		%% log:error reason...
		{reply, {error, <<"permission denied">>}, authorization, State}	
	end;
authorization({'APOP', [_Username, _Digest]}, State) ->
	{reply, {error, <<"Unrecognized authentication type">>}, authorization, State};
authorization({'AUTH', _Args}, State) ->
	{reply, {error, <<"Unrecognized authentication type">>}, authorization, State};
authorization({'CAPA', _Args}, State) ->
	{reply, {error, <<"Unrecognized authentication type">>}, authorization, State};
authorization({'SASL', _Args}, State) ->
	{reply, {error, <<"Unrecognized authentication type">>}, authorization, State};
authorization({_, _Args}, State) ->
	{reply, {error, <<"Unrecognized authentication type">>}, authorization, State}.

%%
transaction({'STAT', []}, State) ->
	Timestamp = ewok_http:date(),
	{reply, {ok, [<<"0 ", "0 ", Timestamp/binary>>]}, transaction, State};
transaction({'LIST', []}, State) ->
	{reply, {ok, [<<"0 Messages">>, <<".">>]}, transaction, State};
transaction({'LIST', [MessageId]}, State) ->
	{reply, {error, <<"No such message id:", MessageId/binary>>}, transaction, State};
transaction({'RETR', [MessageId]}, State) ->
	{reply, {error, <<"No such message id:", MessageId/binary>>}, transaction, State};
transaction({'DELE', [MessageId]}, State) ->
	Length = size(MessageId),
	{reply, {ok, <<"Message ", MessageId:Length/binary, " deleted">>}, transaction, State};
transaction({'NOOP', []}, State) ->
	{reply, {ok, <<>>}, transaction, State};
transaction({'RSET', []}, State) ->
	{reply, {ok, <<"Maildrop has 0 messages (0 octets)">>}, transaction, State};
transaction({'QUIT', []}, State) ->
	{reply, {ok, <<"POP3 server signing off (maildrop empty)">>}, update, State}.

%%
update({'QUIT', []}, State) ->
	{noreply, terminate, State}.



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

-module(ewok_umtp).
-name("Ewok UMTP Service").

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("umtp.hrl").

%% Universal Message Transfer Protocol
%% Suggested IANA Port 30
%% Suggested Well-known ports 3300-3301
%% Stateless
-define(UMTP_PORT, 30).

-export([start/0, start/1]).

-behaviour(ewok_inet).
-export([init/2, terminate/3]).
-export([connected/2]).
%-record(mail, {from, to, timestamp, body}).
-record(state, {remote_ip, remote_port}).

start() ->
	start(?UMTP_PORT).
start(Port) ->
	#ewok_inet{
		transport = tcp,
		protocol = umtp,
		port = Port,
		handler = ewok_umtp,
		codec = ewok_ubf,
		timeout = 10
	}.

%% Callbacks: ewok_inet
init(_Options, {RemoteIP, RemotePort}) ->
	{noreply, connected, #state{remote_ip = RemoteIP, remote_port = RemotePort}}.	
%%
connected(Request, State) ->
	?TTY({umtp, Request}),
	{reply, ok, connected, State}.
%%
terminate(_Reason, _NextState, _StateData) ->
	ok.


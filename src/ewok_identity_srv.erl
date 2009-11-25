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

-module(ewok_identity_srv).
-vsn("1.0").
-author('steve@simulacity.com').

-include("ewok.hrl").

-behaviour(ewok_service).
-export([start_link/0, stop/0, service_info/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(KEYSTORE, ".keystore").

-record(state, {node, clock_seq}).

start_link() ->
	ewok_log:log(default, service, {?MODULE, service_info()}),
	crypto:start(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
	
stop() ->
    gen_server:cast(?SERVER, stop).

service_info() -> [
	{name, "Ewok Identity Service"},
	{version, {1,0,0}},
	{comment, ""}
].

%%
%% uuid gen_server for generating timestamps with saved state
%%

init(Options) ->
    {A1,A2,A3} = proplists:get_value(seed, Options, erlang:now()),
    random:seed(A1, A2, A3),
    State = #state{
        node = proplists:get_value(node, Options, <<0:48>>),
        clock_seq = random:uniform(65536)
    },
    ewok_log:info("uuid server started"),
    {ok, State}.

handle_call(timestamp, _From, State) ->
	%% TODO resolve this...
    Reply = ewok_identity:timestamp(State#state.node, State#state.clock_seq),
    {reply, Reply, State};

handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ewok_log:info("uuid server stopped"),
    ok.


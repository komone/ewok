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

-module(ewok_riak).
-name("Ewok Riak DS").
-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-compile(export_all).

-record(state, {riak_client}).

%%
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% 
stop() ->
    gen_server:cast(?SERVER, stop).

%%
list_keys(Bucket) ->
	gen_server:call(?SERVER, {list_keys, Bucket}, infinity).
%%
lookup(Bucket, Key) ->
	gen_server:call(?SERVER, {get, Bucket, Key}, infinity).
%%
add(Bucket, Key, Value) ->
	gen_server:call(?SERVER, {put, Bucket, Key, Value}, infinity).
%%
update(Bucket, Key, Value) ->
	gen_server:call(?SERVER, {update, Bucket, Key, Value}, infinity).
%%
delete(Bucket, Key) ->
	gen_server:call(?SERVER, {delete, Bucket, Key}, infinity).
%%
info(Bucket) ->
	gen_server:call(?SERVER, {info, Bucket}, infinity).

%%
%%% gen_server
%%
init(_Args) ->
	process_flag(trap_exit, true), % when do we need this?
	{ok, Client} = riak:client_connect('riak@127.0.0.1'),
    {ok, #state{riak_client = Client}}.
%
handle_call({list_keys, B}, _From, State = #state{riak_client = Client}) ->
	Result = Client:list_keys(B),
	{reply, Result, State};	
%
handle_call({get, B, K}, _From, State = #state{riak_client = Client}) ->
	{ok, Object} = Client:get(B, K, 1),
	Result = riak_object:get_value(Object), 
	{reply, Result, State};
%
handle_call({put, B, K, V}, _From, State = #state{riak_client = Client}) ->
	Object = riak_object:new(B, K, V),
	Result = Client:put(Object, 1),
	{reply, Result, State};
%
handle_call({update, B, K, V}, _From, State = #state{riak_client = Client}) ->
	{ok, Object} = Client:get(B, K, 1),
	NewObject = riak_object:update_value(Object, V),
	Result = Client:put(NewObject, 1),
	{reply, Result, State};
%
handle_call({delete, B, K}, _From, State = #state{riak_client = Client}) ->
	Result = Client:delete(B, K, 1),
	{reply, Result, State};	
%
handle_call({info, B}, _From, State = #state{riak_client = Client}) ->
	Result = Client:get_bucket(B),
	{reply, Result, State};
%
handle_call(_Message, _From, State) ->
	{reply, not_implemented, State}.
%
handle_cast(stop, State) ->
    {stop, normal, State};
%
handle_cast(_, State) ->
    {noreply, State}.
%
handle_info(_Info, State) ->
    {noreply, State}.
%
terminate(_Reason, _State) ->
    ok.
%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.
	

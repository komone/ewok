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

-module(ewok_session_srv). 
-name("Ewok Session Service").
-depends([ewok_cache_srv, ewok_scheduler_srv]).

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-record(state, {default_ttl, flush_interval, force_flush}).

-define(SERVER, ?MODULE).
-define(ETS, ?MODULE).
-define(DEPENDS, [ewok_data_srv, ewok_identity_srv]).

%%
%% ewok_service Callbacks
%%
start_link(Args) ->
	ewok_util:check_dependencies(?DEPENDS),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
stop() ->
    gen_server:cast(?SERVER, stop).

%%
%% gen_server Callbacks
%%
init([]) ->
    ets:new(?ETS, [set, named_table, public, {keypos, 2}]),
	TTL = ewok_config:get_value({ewok, http, session, timeout}, 1800),
	IdleTimeout = ewok_config:get_value({ewok, http, session, flush, interval}, 120) * 1000,
	ForceTimeout = ewok_config:get_value({ewok, http, session, flush, force}, 3600) * 1000,
	State = #state{default_ttl=TTL, flush_interval=IdleTimeout, force_flush=ForceTimeout},
	% ?TTY({?MODULE, State}),
    schedule_timeout(State),
    {ok, State, State#state.flush_interval}.

%%
handle_call({create_session, IP, TTL, Pid}, _From, State) ->
    Timestamp = calendar:universal_time(),
	Now = ewok_util:unow(),
	TTL1 = 
		case TTL of 
		default_ttl -> State#state.default_ttl;
		X when is_integer(X) -> TTL
		end,
    Key = ewok_identity:key(),
	Session = #ewok_session {
		key = Key,
		ip = IP,
		user = undefined,
		started = Timestamp,
		expires = Now + TTL1,
		ttl = TTL1,
		notify = Pid },
    ets:insert_new(?ETS, Session),
    {reply, Session, State, State#state.flush_interval};
%
handle_call({delete_session, S = #ewok_session{key=Key}}, _From, State) ->
	ets:delete(?ETS, Key),
	notify(S, normal),
	{reply, ok, State, State#state.flush_interval}.

%%
handle_cast(stop, State) ->
    {stop, normal, State};
%
handle_cast(_Msg, State) ->
    {noreply, State, State#state.flush_interval}.

%%
handle_info(timeout, State) ->
    cleanup(),
    {noreply, State, State#state.flush_interval};
handle_info(scheduled_timeout, State) ->
    cleanup(),
    schedule_timeout(State),
    {noreply, State, State#state.flush_interval}.

%%
terminate(_Reason, _State) ->
    ok.
	
%%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.	

%%% internal functions

%%% TODO: should this be changed to use ewok_scheduler?
schedule_timeout(#state{force_flush = ForceFlush}) ->
    erlang:send_after(ForceFlush, self(), scheduled_timeout).

%%
cleanup() -> 
	cleanup(ewok_util:unow(), ets:first(?ETS)).
%%
cleanup(_, '$end_of_table') -> 
	ok;
%
cleanup(Now, Key) ->
    case ets:lookup(?ETS, Key) of
	[Session] ->
		case Session#ewok_session.expires > Now of 
		true -> 
			cleanup(Now, ets:next(?ETS, Key));
		false -> 
			notify(Session, timeout),
			Next = ets:next(?ETS, Key),
			ets:delete(?ETS, Key),
			cleanup(Now, Next)
		end;
	[] ->
		cleanup(Now, ets:next(?ETS, Key))
    end.

%%
notify(#ewok_session{notify = Pid, key = SessionKey}, Type) ->
    try 
		Pid ! {session_end, Type, SessionKey}
	catch
	_:_ ->
		ewok_log:warn({notify_failed, Pid, SessionKey})
	end.

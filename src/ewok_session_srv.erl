%%
%%
%%
-module(ewok_session_srv). 
-author('steve@simulacity.com').

-include("../include/ewok.hrl").

-behaviour(ewok_service).
-export([start_link/0, stop/0, service_info/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

%% API
-export([get_session/2]).
-export([update_session/1, close_session/1]).
-export([sessions/0]).

-define(SERVER, ?MODULE).
-define(ETS, ?MODULE).

-define(EWOK_SESSION_KEY, <<"_EWOKSID">>).

-record(state, {default_ttl, flush_interval, force_flush}).

%% The ETS record
-record(ewok_session, {key, ip, user, data=[], started, expires, ttl, notify}).

%%
%% API
%%

%%
get_session(Cookie, RemoteIp) ->
	Record = 
		case proplists:get_value(?EWOK_SESSION_KEY, Cookie) of
		undefined -> 
			new_session(RemoteIp);
		SessionKey ->
			case ets:lookup(?ETS, SessionKey) of
			[] -> 
				new_session(RemoteIp);
			[Value] when is_record(Value, ewok_session) ->  
				%% NOTE: check that the IP is valid, if it isn't then return a new session
				case Value#ewok_session.ip of
				RemoteIp -> Value;
				_ -> new_session(RemoteIp)
				end
			end
		end,
	%% IMPORTANT! This instantiation MUST be done in the calling process, not in the remote gen_server process
	Key = Record#ewok_session.key,		
	Session = ewok_session_obj:new(
		Key,
		Record#ewok_session.ip,
		Record#ewok_session.started,
		Record#ewok_session.expires,
		Record#ewok_session.ttl,
		Record#ewok_session.notify,
		make_cookie(Key)
	),
	Session:init(Record#ewok_session.user, Record#ewok_session.data),
	Session.
%%
new_session(RemoteIp) ->
	gen_server:call(?SERVER, {create_session, RemoteIp, default_ttl, self()}, infinity).
	

%% This only needs done once...
make_cookie(Key) ->
	%% NOTE: for -> cookie2...
	%% Expiry = ewok_util:date(calendar:gregorian_seconds_to_datetime(Expires)),	
	Cookie = [
		?EWOK_SESSION_KEY, <<"=">>, Key,
		<<";Version=1">>, 
		%% Allow browser to discard the cookie, i.e. don't set Max-Age
		%% Later this may be used for "remember me", but then the session
		%% needs to be persisted in ewok_db also
		%% <<";Max-Age">>, integer_to_list(TTL),
		<<";Path=/">>
	],
	list_to_binary(Cookie).
		
%%
close_session(Session) -> 
	gen_server:call(?SERVER, {delete_session, Session:value()}, infinity).

%% IMPL: This call MUST be kept in sync with any changes to the session record.
% -record(session, {key, ip, user, data=[], started, expires, ttl, notify}).
%% update_element takes the table lock so this should be safe to do from the client process?
update_session(Session) ->
	{ewok_session, Key, _, User, Data, _, _Expires, TTL, _} = Session:value(),
	ets:update_element(?ETS, Key, [{4, User}, {5, Data}, {7, unow() + TTL}]).

%
sessions() ->
    ets:tab2list(?ETS).

%%
%% ewok_service Callbacks
%%
start_link() ->
	ewok_log:log(default, service, {?MODULE, service_info()}),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() ->
    gen_server:cast(?SERVER, stop).

service_info() -> [ 
	{name, "Ewok Session Service"},
	{version, {1,0,0}},
	{depends, [ewok_cache_srv, ewok_scheduler_srv]}
].

%%
%% gen_server Callbacks
%%
init([]) ->
    ewok_identity:seed(),
    ets:new(?ETS, [set, named_table, public, {keypos, 2}]),
	TTL = ewok:config({ewok, http, session, timeout}, 1800),
	IdleTimeout = ewok:config({ewok, http, session, flush, interval}, 120) * 1000,
	ForceTimeout = ewok:config({ewok, http, session, flush, force}, 3600) * 1000,
	State = #state{default_ttl=TTL, flush_interval=IdleTimeout, force_flush=ForceTimeout},
%	?TTY("~p: ~p~n", [?MODULE, State]),
    schedule_timeout(State),
    {ok, State, State#state.flush_interval}.

%%
handle_call({create_session, IP, TTL, Pid}, _From, State) ->
    Timestamp = calendar:universal_time(),
	Now = calendar:datetime_to_gregorian_seconds(Timestamp),
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
	
%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.	

%%
%%% internal functions
%%
unow() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).

%%% TODO: should this be changed to use ewok_scheduler?
schedule_timeout(State) when is_record(State, state) ->
    erlang:send_after(State#state.force_flush, self(), scheduled_timeout).

%%
cleanup() -> 
	cleanup(unow(), ets:first(?ETS)).
%
cleanup(_, '$end_of_table') -> 
	ok;
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

%
notify(S, Type) when is_record(S, ewok_session) ->
    case S#ewok_session.notify of
	undefined -> ok;
	Pid -> Pid ! {session_end, Type, S#ewok_session.key}
    end.

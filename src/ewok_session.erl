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

-module(ewok_session).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl").

%% API
-export([get_session/2]).
-export([update/1, close/1]).
-export([list/0]).

-define(SERVER, ewok_session_srv).
-define(ETS, ewok_session_srv).

%% API

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
	
%% IMPL: This call MUST be kept in sync with any changes to the session record.
% -record(session, {key, ip, user, data=[], started, expires, ttl, notify}).
%% update_element takes the table lock so this should be safe to do from the client process?
update(Session) ->
	{ewok_session, Key, _, User, Data, _, _Expires, TTL, _} = Session:value(),
	ets:update_element(?ETS, Key, [{4, User}, {5, Data}, {7, ewok_util:unow() + TTL}]).

%%
close(Session) -> 
	gen_server:call(?SERVER, {delete_session, Session:value()}, infinity).

%
list() ->
    ets:tab2list(?ETS).

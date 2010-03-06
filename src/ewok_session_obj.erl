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

-module(ewok_session_obj, [ID, IP, Started, Expires, TTL, Notify, Cookie]).

%-include("../include/ewok.hrl").

-export([init/2, reset/0, value/0]).
-export([key/0, ip/0, user/0, data/0, started/0, expires/0, ttl/0, cookie/0]).
-export([user/1, read/1, read/2, save/2, take/1]).

%% IMPORTANT NOTE: This rather unusual (and dangerous) design simplifies the API considerably.
%% It relies on the fact that a particular session is only ever updated by a single process...
%% the question is... is this assumption *absolutely true*??? If not - another question arises
%% which is: would it matter?

%-record(session, {id, ip, user, data=[], started, expires, ttl, notify}).

-define(USER, www_session_user).
-define(DATA, www_session_data).

key() -> ID.
ip() -> IP.
started() -> Started.
expires() -> Expires.
ttl() -> TTL.
% notify() -> get(?NOTIFY).
cookie() -> Cookie.

init(User, Data) ->
	put(?USER, User),
	put(?DATA, Data).
%
reset() -> [erase(Value) || Value <- [?USER, ?DATA]].

%
user() -> get(?USER).
%
user(User) ->
	%% only allow this to be set once...?
	undefined = put(?USER, User),
	%put(?USER, User),
	ok.

%
data() -> get(?DATA).

%
save(Key, Value) ->
	put(?DATA, lists:keystore(Key, 1, data(), {Key, Value})),
	ok.
%
read(Key, Default) ->
	case read(Key) of
	undefined -> {Key, Default};
	Value -> Value
	end.
%
read(Key) ->
	case lists:keyfind(Key, 1, data()) of
	false -> undefined;
	Value -> Value
	end.
%
take(Key) ->
	case lists:keytake(Key, 1, data()) of
	{value, Value, Data} ->
		put(?DATA, Data),
		Value;
	false -> undefined
	end.
%
value() ->
	{ewok_session, ID, IP, get(?USER), get(?DATA), Started, Expires, TTL, Notify}.

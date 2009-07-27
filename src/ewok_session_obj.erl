%%
-module(ewok_session_obj, [ID, IP, Started, Expires, TTL, Notify, Cookie]).

%-include("ewok.hrl").

-export([init/2, reset/0, value/0]).
-export([key/0, ip/0, user/0, data/0, started/0, expires/0, ttl/0, cookie/0]).
-export([user/1, read/1, read/2, save/1, take/1]).

%% IMPORTANT NOTE: This rather unusual (and dangerous) design simplifies the API considerably.
%% It relies on the fact that a particular session is only ever updated by a single process...
%% the question is... is this assumption *absolutely true*??? If not - another question arises
%% which is: would it matter?

%-record(session, {key, ip, user, data=[], started, expires, ttl, notify}).
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
	put(?USER, User),
	ok.
	%% or only allow this to be set once... is this correct?
	% undefined = put(?USER, User).

%
data() -> get(?DATA).

%
save(Data = {Key, _}) ->
	put(?DATA, lists:keystore(Key, 1, data(), Data)),
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

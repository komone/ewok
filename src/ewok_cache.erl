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

-module(ewok_cache).
-include("ewok.hrl").

%% Cache API
-export([add/1, remove/1, clear/1, lookup/1, lookup/2]).

-define(SERVER, ewok_cache_srv).

%
add(Records) when is_list(Records) ->
	X = [add(X) || X <- Records],
	{ok, length(X)};
add(Record) when ?is_record(Record) ->
	gen_server:call(?SERVER, {add, Record}, infinity).

remove(Records) when is_list(Records) ->
	X = [remove(X) || X <- Records],
	{ok, length(X)};
remove(Record) when ?is_record(Record) ->
	gen_server:call(?SERVER, {remove, Record}, infinity).

clear(Type) when is_atom(Type) ->
	gen_server:call(?SERVER, {clear, Type}, infinity).

lookup(Type) ->
	case lists:member(Type, ets:all()) of
	true -> lists:sort(ets:tab2list(Type));
	false -> undefined
	end.
lookup(Type, Key) ->
	case lists:member(Type, ets:all()) of
	true ->
		case ewok_cache_srv:lookup(Type, Key) of
		[R] -> R;
		[] -> undefined 
		end;
	false -> undefined
	end.

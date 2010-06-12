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

-module(ewok_identity).

-include("ewok.hrl").
-include("ewok_system.hrl").

-export([id/0, id/1, key/0, uuid/0, timestamp/0]).
-export([random/0, password/1, keystore/0, keystore/1, keystore/2]). %% temporary

-define(SERVER, ewok_identity_srv).


%% API
%%
timestamp() ->
    gen_server:call(?SERVER, timestamp).

random() ->
	case is_pid(whereis(?SERVER)) of
	true ->
		id();
	false ->
		{A1, A2, A3} = now(),
		random:seed(A1, A2, A3),
		ewok_identity_srv:random()
	end.
	
%%
id() ->  id(default).
%
id(default)   -> gen_server:call(?SERVER, {new, default});
id(md5)       -> gen_server:call(?SERVER, {new, md5});
id(sha)       -> gen_server:call(?SERVER, {new, sha});
id(timestamp) -> gen_server:call(?SERVER, {new, timestamp}).

%%
key() -> 
	ewok_hex:encode(id()).
%%
uuid() -> 
	<<A:64, B:32, C:32, D:32, E:96>> = key(),
	<<A:64, $-, B:32, $-, C:32, $-, D:32, $-, E:96>>.

%%
password(Password) when is_binary(Password) ->
	gen_server:call(?SERVER, {password, Password}).	
%%
keystore() ->
	gen_server:call(?SERVER, {keystore}).
%%
keystore(Key) ->
	gen_server:call(?SERVER, {keystore, Key}).
%%
keystore(Key, Value) ->
	gen_server:call(?SERVER, {keystore, Key, Value}).


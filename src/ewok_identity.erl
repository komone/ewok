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

-export([id/0, id/1, key/0, uuid/0, timestamp/0, keystore/0]).
-export([random/0]). %% temporary

-define(SERVER, ewok_identity_srv).

%% TODO: Replace this with an id that's dynamically generated at build time
-define(IVEC, <<213,53,164,93,158,212,70,56,134,80,224,220,249,214,82,76>>).

%% API
%%
timestamp() ->
    gen_server:call(?SERVER, timestamp).

random() ->
	{A1, A2, A3} = now(),
	random:seed(A1, A2, A3),
    <<
        (random:uniform(4294967296) - 1):32,
        (random:uniform(4294967296) - 1):32,
        (random:uniform(4294967296) - 1):32,
        (random:uniform(4294967296) - 1):32
    >>.	

%%
id() ->  id(default).
%
id(default)   -> gen_server:call(?SERVER, {new, default});
id(md5)       -> gen_server:call(?SERVER, {new, md5});
id(sha)       -> gen_server:call(?SERVER, {new, sha});
id(timestamp) -> gen_server:call(?SERVER, {new, timestamp}).

%%
key() -> 
	ewok_util:hex(id()).
%%
uuid() -> 
	<<A:64, B:32, C:32, D:32, E:96>> = key(),
	<<A:64, $-, B:32, $-, C:32, $-, D:32, $-, E:96>>.

%%
keystore() ->
	Path = ewok:config({ewok, identity, keystore}, ?DATA_DIR),
	File = ewok_file:path([code:lib_dir(ewok), Path, ?KEYSTORE_FILE]),
	case ewok_file:is_regular(File) of
	true ->	
		{ok, Term} = file:consult(binary_to_list(File)),
		Term;
	false ->
		{error, no_keystore}
	end.

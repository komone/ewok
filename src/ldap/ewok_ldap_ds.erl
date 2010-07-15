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

-module(ewok_ldap_ds).
%-module(ewok_directory).?

%-behaviour(ewok_datasource).
-export([init/1, info/0, info/1, create/1, drop/1, find/2, save/3, run/1]).
-export([datasource_info/0, start_link/1, stop/0]).

% pend remove
datasource_info() -> info().
start_link([]) -> ok.
stop() -> ok.

init([]) -> ok.
info() -> ok.
info(_Key) -> ok.
create(_Table) -> ok.
drop(_Table) -> ok.
find(_Table, _Key) -> ok.
save(_Table, _Key, _Value) -> ok.
run(_Query) -> ok.

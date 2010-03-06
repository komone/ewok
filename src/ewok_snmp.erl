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

-module(ewok_snmp).
-name("Ewok SNMP").

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-export([info/0, digest/2]).

%% http://www.trapexit.org/SNMP_Quick_Start

%% http://www.oid-info.com
%% iso(1) identified-organization(3) dod(6) internet(1) private(4) enterprise(1) simulacity(33850)}
%% Request-37905
-define(PEN, <<37905:32>>).
-define(OID, <<1:32, 3:32, 6:32, 1:32, 4:32, 1:32, 37905:32>>).
-define(SERVER, ?MODULE).
-record(state, {}).

%%
start_link(Args) ->
	ewok_util:check_dependencies([ewok_identity_srv]),
	crypto:start(),
	SnmpOpts = prep_snmp(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, [SnmpOpts | Args], []).
	
%%
stop() ->
    gen_server:cast(?SERVER, stop).
%%
info() ->
	gen_server:call(?SERVER, info).

%%
init(_Opts) ->
	_SnmpAgentAuth = ewok_identity:keystore(snmp_agent_auth),
	_SnmpAgentPriv = ewok_identity:keystore(snmp_agent_priv),
	_SnmpManagerAuth = ewok_identity:keystore(snmp_manager_auth),
	_SnmpManagerPriv = ewok_identity:keystore(snmp_manager_priv),
	
	{ok, #state{}}.

%%
handle_call(info, _From, State) ->
	{reply, State, State};
%%	
handle_call(_Message, _From, State) ->
	{reply, not_implemented, State}.
%%
handle_cast(stop, State) ->
    {stop, normal, State};
%%
handle_cast(_, State) ->
    {noreply, State}.
%%
handle_info(_Info, State) ->
    {noreply, State}.
%%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.
%%
terminate(_Reason, _State) ->
    ok.

%%
digest(PassKey, Engine) ->
	snmp:passwd2localized_key(md5, PassKey, Engine).

%%
prep_snmp() ->
	ok = application:load(snmp),
	
	application:set_env(snmp, agent, [
		{config, [{dir, path(<<"agent/conf">>)}]}, {db_dir, path(<<"agent/db">>)}
	]),
	%% ...more
	
	%% NOTE: Manager uses a different tuple structure to Agent (db_dir is inside the config tuple).
	application:set_env(snmp, manager, [
		{config, [{dir, path(<<"manager/conf">>)}, {db_dir, path(<<"manager/db">>)}]}
	]),
	%% ...more
	
	ok = application:start(snmp),
	Opts = [],
	Opts.

path(Bin) ->
	DataDir = ewok_config:get_value({ewok, data_dir}, ?DATA_DIR),
	binary_to_list(ewok_file:path([DataDir, <<"snmp">>, Bin])).

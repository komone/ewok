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

-module(ewok_installer).
-name("Ewok Installer").

-include("ewok_system.hrl").
-include("ewok.hrl").
-include("esp.hrl").
%%
-export([validate/0, install/0, run/1]).

-behaviour(ewok_http_resource).
-export([filter/1, resource_info/0]).
-export(['GET'/2]).

%% TODO: Perhaps have the services define the tables they require?	
-define(TABLES, [
	{ewok_auth, record_info(fields, ewok_auth)},
	{ewok_config, record_info(fields, ewok_config)},
	{ewok_mimetype, record_info(fields, ewok_mimetype)},
	{ewok_profile, record_info(fields, ewok_profile)},
	{ewok_role, record_info(fields, ewok_role)},
	{ewok_route, record_info(fields, ewok_route)},
	{ewok_session, record_info(fields, ewok_session)},
	{ewok_task, record_info(fields, ewok_task)},
	{ewok_user, record_info(fields, ewok_user)}
]).

%%
validate() ->
	run(false).

%%
install() ->
	run(true).

%% @private
run(Autoinstall) ->
	DataDir = ewok_util:get_env(data_dir, ?DATA_DIR),
	EwokDir = ewok_file:path([ewok_util:appdir(), DataDir, "Ewok." ++ atom_to_list(node())]),
	
	MnesiaRunning = mnesia:system_info(is_running),
	MnesiaUseDir = mnesia:system_info(use_dir),
	MnesiaDir = ewok_file:path(mnesia:system_info(directory)),
	
	%% NOTE: The following conditions should be managed extremely carefully
	case {MnesiaRunning, MnesiaUseDir, MnesiaDir, Autoinstall} of 
	{yes, true, EwokDir, _} ->
		check_tables(Autoinstall);
	{yes, false, EwokDir, true} ->
		stopped = mnesia:stop(),
		ok = mnesia:create_schema([node()]),
		ok = mnesia:start(),
		check_tables(Autoinstall);
	{yes, false, EwokDir, _} ->
		{error, {mnesia, ram_copy_only}};
	{yes, _, _, _} -> 
		{error, {mnesia, already_in_use}};
	{no, true, EwokDir, _} ->
		ok = mnesia:start(),
		check_tables(Autoinstall);
	{no, _, _, true} ->
		ok = application:set_env(mnesia, dir, binary_to_list(EwokDir)),
		case mnesia:system_info(use_dir) of
		true ->
			ok;
		false ->
			ok = mnesia:create_schema([node()])
		end,
		ok = mnesia:start(),
		check_tables(Autoinstall);
	{no, false, EwokDir, _} ->
		{error, {mnesia, ram_copy_only}};
	{no, _, _, _} ->
		{error, {mnesia, not_configured}};
	{Other, _, _, _} ->
		{error, {mnesia, Other}}
	end.

%% @private
check_tables(Autoinstall) ->
	Existing = ewok_db:tables(),
	mnesia:wait_for_tables(Existing, 10000),
	
	Missing = [Table || Table = {X, _} <- ?TABLES, not lists:member(X, Existing)],
	case Missing of
	[] ->
		check_data(Autoinstall);
	_ when Autoinstall =:= true ->
		Created = [ewok_db:create_table(Name, Attrs) || {Name, Attrs} <- Missing],
		Failed = [X || X <- Created, X =/= ok],
		case Failed of 
		[] -> 
			check_data(Autoinstall);
		_ -> 
			{error, Failed}
		end;
	_ when Autoinstall =:= false ->
		{error, {tables_missing, Missing}}
	end.

%%
check_data(Autoinstall) ->
	case ewok_db:select(ewok_user, {name, {ewok, "admin"}}) of
	{ok, [User]} -> 
		Auth = ewok_db:read(ewok_auth, User#ewok_user.id),
		{ok, Auth#ewok_auth.activation};
	{ok, []} -> 
		case Autoinstall of
		true ->
			init_user_table();
		false ->
			{error, no_admin_user}
		end
	end.

%%
init_user_table() ->
	AdminName = {ewok, "admin"},
	AdminRole = {ewok, admin},
	UserID = ewok_identity:random(),
	Activation = ewok_util:hex(ewok_identity:random()),
	%%
	ok = ewok_db:create(#ewok_role{id=AdminRole}),
	ok = ewok_db:create(#ewok_user{id=UserID, name=AdminName, roles=[AdminRole]}), 
	ok = ewok_db:create(#ewok_auth{id=UserID, activation=Activation}),
	{ok, {AdminName, Activation}}.

%%
%% ewok_http_resource callbacks
%%
resource_info() -> [].

%%
filter(_Request) ->  ok.

%%
'GET'(_Request, _Session) -> 
	case validate() of 
	true ->
		{ok, [{content_type, "text/plain"}], ewok_http:text(get_install_props())};
	false ->
		{found, [{location, "/activation"}], []}
	end.

%%
get_install_props() ->
	Node = node(),
	IsRemote = (Node =/= 'nonode@nohost'),
	UsesDb = ([Node] =:= [N || DbNode = N <- mnesia:system_info(db_nodes), DbNode =:= Node]),
	HasDir = mnesia:system_info(directory),
	UsesDir = mnesia:system_info(use_dir),
	[{node, Node}, {remote, IsRemote}, {db_node, UsesDb}, {db_dir, HasDir}, {db_used, UsesDir}].

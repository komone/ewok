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
-module(ewok_db).
-name("Ewok DB").

-include("ewok.hrl").
-include("ewok_system.hrl").
-include_lib("stdlib/include/qlc.hrl").

%-behaviour(ewok_service).
-export([start/0, start/1, stop/0]).

%-behaviour(ewok_datasource).
-export([connect/0, datasource_info/0, metadata/0, metadata/1, table_info/1,
	create/1, read/1, read/2, update/1, delete/1, select/1, run/1, 
	create_table/2, drop_table/1]).
	
-export([size/1, add/1, remove/1, lookup/2, find/1, select/2, tables/0, fields/0]).
%% development mode only?
-export([create_tables/1, create_missing/1, drop_tables/0]).


%% TODO: Perhaps have the services define the tables they require?	
-define(TABLES, [
	{ewok_auth, record_info(fields, ewok_auth)},
	{ewok_config, record_info(fields, ewok_config)},
	{ewok_mimetype, record_info(fields, ewok_mimetype)},
	{ewok_profile, record_info(fields, ewok_profile)},
	{ewok_role, record_info(fields, ewok_role)},
	{ewok_route, record_info(fields, ewok_route)},
	{http_session, record_info(fields, http_session)},
	{ewok_task, record_info(fields, ewok_task)},
	{ewok_user, record_info(fields, ewok_user)}
]).
% temp
fields() ->
	?TABLES.
	
%%
start() ->
	start([]).
%%
start(Opts) ->
	Autoinstall = proplists:get_value(autoinstall, Opts, true),
	DataDir = proplists:get_value(data_dir, Opts, ?DATA_DIR),
	EwokDir = ewok_file:path([ewok_util:appdir(), DataDir, "mnesia/Ewok." ++ atom_to_list(node())]),
	
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
	Existing = tables(),
	mnesia:wait_for_tables(Existing, 10000),
	
	Missing = [Table || Table = {X, _} <- ?TABLES, not lists:member(X, Existing)],
	case Missing of
	[] ->
		check_data(Autoinstall);
	_ when Autoinstall =:= true ->
		Created = [create_table(Name, Attrs) || {Name, Attrs} <- Missing],
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
	case select(ewok_user, {name, ?ADMIN_USER}) of
	{ok, [User]} -> 
		Auth = read(ewok_auth, User#ewok_user.id),
		case Auth#ewok_auth.password of
		undefined ->
			{ok, ?ADMIN_USER, Auth#ewok_auth.activation};
		_ -> 
			ok
		end;
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
	UserID = ewok_identity:random(),
	Activation = ewok_hex:encode(ewok_identity:random()),
	%%
	ok = create(#ewok_role{id=?ADMIN_ROLE}),
	ok = create(#ewok_user{id=UserID, name=?ADMIN_USER, roles=[?ADMIN_ROLE]}), 
	ok = create(#ewok_auth{id=UserID, activation=Activation}),
	{ok, ?ADMIN_USER, Activation}.

%%
stop() ->
	mnesia:stop().

%%
connect() ->
	Running = 
		case mnesia:system_info(is_running) of
		yes -> true;
		_ -> false
		end,
	#datasource {id = mnesia, name = "Ewok DB", mod = ?MODULE,
		running = Running, valid = true,
		data = [{tables, tables()}]}.	

%% required... 
datasource_info() -> [].
metadata() -> [].
metadata(_Key) -> [].
read({Table, Key}) -> read(Table, Key).
run(_Query) -> ok.
table_info(Table) -> mnesia:table_info(Table, all).

tables() ->
	mnesia:system_info(tables).
size(Table) ->
	mnesia:table_info(Table, size).

%% Aliases
add(Records) -> create(Records).
remove(Records) -> delete(Records).
lookup(Type, Key) -> read(Type, Key). 
find(Match) ->	select(Match).

%%
create([]) ->
	ok;
create([H|T]) ->
	create(H), 
	create(T);
create(Record) when is_tuple(Record) ->
	Table = element(1, Record),
	Key = element(2, Record),
    T = fun () -> 
		case mnesia:wread({Table, Key}) of 
		[] -> 
			mnesia:write(Record);
		_ -> 
			{error, exists}
		end
	end,
    {atomic, Result} = mnesia:transaction(T),
    Result.
%%
read(Table, Key) when is_atom(Table) ->
	T = fun () -> mnesia:read({Table, Key}) end,
    case mnesia:transaction(T) of
    {atomic, [Result]} -> 
		Result;
	_ -> 
		undefined
	end.
%%	
update(Record) when is_tuple(Record) ->
    T = fun () -> mnesia:write(Record) end,
    {atomic, Result} = mnesia:transaction(T),
    Result.
%%	
delete([]) ->
	ok;
delete([H|T]) ->
	delete(H), delete(T);
delete(Record) when is_tuple(Record) ->
	delete(element(1, Record), element(2, Record)).
delete(Table, Key) when is_atom(Table) ->
	T = fun () -> mnesia:delete({Table, Key}) end,
	{atomic, Result} = mnesia:transaction(T),
	Result.
	
%% TODO review use of match_object -> mnesia:select
select(Table) when is_atom(Table) -> 
	T = fun () ->
		Pattern = mnesia:table_info(Table, wild_pattern),
		mnesia:match_object(Pattern)
		end,
	{atomic, Result} = mnesia:transaction(T),
	{ok, Result};
%%
select(Match) when is_tuple(Match) ->
	T = fun() -> 
			mnesia:match_object(Match)
		end,
	{atomic, Result} = mnesia:transaction(T),
	{ok, Result}.

select(Table, {Field, Value}) when is_atom(Field) ->
    T = fun () -> 
			Spec = mnesia:table_info(Table, attributes),
			Match = make_match({Table}, {Field, Value}, Spec),
			%io:format("MATCH ON: ~p~n", [Match]),
			mnesia:match_object(Match) 
		end,
	{atomic, Result} = mnesia:transaction(T),
	{ok, Result};
	
select(Table, _Num) when is_atom(Table) ->
	T = fun () ->
		qlc:e(qlc:q([X || X <- mnesia:table(Table)]))
	end,
	{atomic, Result} = mnesia:transaction(T),
	{ok, Result}.

make_match(Record, {Field, Value}, [H|T]) when Field =:= H ->
	make_match(erlang:append_element(Record, Value), {Field, Value}, T);
make_match(Record, {Field, Value}, [_|T]) ->
	make_match(erlang:append_element(Record, '_'), {Field, Value}, T);
make_match(Record, _, []) ->
	Record.

%%
%% Development and installation use only
%% We should assume Mnesia is running
%%
create_missing(Tables) ->
	Existing = tables(),
	Missing = [Table || Table = {Name, _Fields} <- Tables, not lists:member(Name, Existing)],
	ok = create_tables(Missing),
	{ok, Missing}.
	
create_table(Table, Fields) when is_atom(Table), is_list(Fields) ->
	{atomic, ok} = mnesia:create_table(Table, [{disc_copies, [node()]}, {attributes, Fields}]),
    ok = mnesia:wait_for_tables([Table], 10000).

create_tables([{K, V}|T]) ->
	create_table(K, V), 
	create_tables(T);
create_tables([]) -> 
	ok.
 
drop_table(Table) when is_atom(Table) ->
	drop_tables([Table]).

drop_tables() ->
	Tables = [X || X1 = X <- mnesia:system_info(tables), X1 =/= schema],
	% drop_tables(Tables).
	delete_tables(Tables, []). %% TODO: remove when config is sorted
	
drop_tables(Tables) when is_list(Tables) -> % this should require admin privileges
	case lookup(ewok_config, {ewok, runmode}) of
	#ewok_config{value = development} -> 
		delete_tables(Tables, []);
	#ewok_config{value = Mode} -> 
		{error, {runmode, Mode}}
	end.
	
% @private
delete_tables([H|T], Acc) ->
	{atomic, ok} = mnesia:delete_table(H),
	delete_tables(T, [H|Acc]);
delete_tables([], Acc) ->
	{ok, Acc}.

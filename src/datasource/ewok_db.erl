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

-module(ewok_db).
-name("Ewok DB").
-include("ewok.hrl").
-include("ewok_system.hrl").
-include_lib("stdlib/include/qlc.hrl").

-export([start/0, stop/0, tables/0]).
%% misc
-export([size/1, add/1, remove/1, lookup/2, select/2, create_tables/1, drop_tables/0]).

-behaviour(ewok_datasource).
-export([init/1, datasource_info/0, metadata/0, metadata/1, table_info/1,
	create/1, read/1, read/2, update/1, delete/1, select/1, run/1, 
	create_table/2, drop_table/1]).

%%
start() ->
	DataDir = ewok_file:path([ewok_util:appdir(), ewok_util:get_env(data_dir, ?DATA_DIR)]),
	EwokDataDir = ewok_file:path([DataDir, "Ewok." ++ atom_to_list(node())]),
	MnesiaRunning = mnesia:system_info(is_running),
	MnesiaUseDir = mnesia:system_info(use_dir),
	MnesiaDataDir = ewok_file:path(mnesia:system_info(directory)),
	case {MnesiaRunning, MnesiaUseDir, MnesiaDataDir} of
	{yes, true, EwokDataDir} ->
		{ok, connect()};
	{yes, _, _} ->
		{error, {mnesia, already_in_use}};
	{no, _, _} ->
		ok = application:set_env(mnesia, dir, binary_to_list(EwokDataDir)),
		case mnesia:system_info(use_dir) of
		true ->
			ok = mnesia:start(),
			mnesia:wait_for_tables(tables(), 10000),
			{ok, connect()};
		false ->
			{error, {mnesia, no_schema, EwokDataDir}}
		end;
	{Other, _, _} ->
		{error, {mnesia, Other}}
	end.

%%
stop() ->
	mnesia:stop(),
	application:unload(mnesia). %% needed?

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
init(_Opts) -> ok.
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
		[] -> mnesia:write(Record);
		_ -> {error, exists}
		end
	end,
    {atomic, Result} = mnesia:transaction(T),
    Result.
%%
read(Table, Key) when is_atom(Table) ->
	T = fun () -> mnesia:read({Table, Key}) end,
    case mnesia:transaction(T) of
    {atomic, [Result]} -> Result;
	_ -> undefined
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
create_table(Table, Fields) when is_atom(Table), is_list(Fields) ->
	{atomic, ok} = mnesia:create_table(Table, [{disc_copies, [node()]}, {attributes, Fields}]),
    ok = mnesia:wait_for_tables([Table], 10000).

create_tables([{K, V}|T]) ->
	create_table(K, V), 
	create_tables(T);
create_tables([]) -> ok.
 
drop_table(Table) when is_atom(Table) ->
	drop_tables([Table]).

drop_tables() ->
	Tables = [X || X1 = X <- mnesia:system_info(tables), X1 =/= schema],
	% drop_tables(Tables).
	delete_tables(Tables, []). %% TODO: remove when config is sorted
	
drop_tables(Tables) when is_list(Tables) -> % this should require admin privileges
	case ewok:config({ewok, runmode}) of
	development -> 
		delete_tables(Tables, []);
	Mode -> 
		{error, {runmode, Mode}}
	end.
% @private
delete_tables([H|T], Acc) ->
	{atomic, ok} = mnesia:delete_table(H),
	delete_tables(T, [H|Acc]);
delete_tables([], Acc) ->
	{ok, Acc}.

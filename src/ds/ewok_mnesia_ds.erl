%%  
-module(ewok_mnesia_ds, []).
-vsn("1.0.0").
-author('steve@simulacity.com').

-include("ewok.hrl").
-include("ewok_system.hrl").

-include_lib("stdlib/include/qlc.hrl").

-behaviour(ewok_datasource).
-export([init/1, datasource_info/0, metadata/0, metadata/1, table_info/1,
	create/1, read/1, update/1, delete/1, select/1, run/1, 
	create_table/2, drop_table/1]).

%% TODO: Continuations...

%%
datasource_info() -> [
	{name, "Mnesia"},
	{version, mnesia:system_info(version)}
]. %% ...more!

%% TODO: Ensure mnesia is really valid/running etc.
init(_Args) ->
	{ok, #datasource {
		id = mnesia,
		name = "Mnesia",
		mod = ?MODULE,
		running = mnesia:system_info(is_running),
		valid = true,
		data = [{tables, mnesia:system_info(tables)}]
	}}.

%%
metadata() -> 
	metadata(all).
metadata(Key) ->
	%% for now...
	mnesia:system_info(Key).

%%
table_info(Table) when is_atom(Table) ->
	table_info({Table, all});
table_info({Table, Key}) ->
	mnesia:table_info(Table, Key).

%%
create(Record) when is_tuple(Record) ->
	Table = element(1, Record),
	Key = element(2, Record),
    T = fun () -> 
		case mnesia:wread({Table, Key}) of 
		[] -> mnesia:write(Record);
		_ -> exists
		end
	end,
    {atomic, Result} = mnesia:transaction(T),
    Result.
%%
read({Table, Key}) when is_atom(Table) ->
	T = fun () -> mnesia:read({Table, Key}) end,
    case mnesia:transaction(T) of
    {atomic, [Result]} -> Result;
	_ -> []
	end.
%%	
update(Record) when is_tuple(Record) ->
    T = fun () -> mnesia:write(Record) end,
    {atomic, Result} = mnesia:transaction(T),
    Result.
%%	
delete({Table, Key}) when is_atom(Table) ->
	T = fun () -> mnesia:delete({Table, Key}) end,
	{atomic, Result} = mnesia:transaction(T),
	Result;
delete(Record) when is_tuple(Record) ->
	delete({element(1, Record), element(2, Record)}).

%%
select(Table) when is_atom(Table) -> 
	T = fun () ->
		Pattern = mnesia:table_info(Table, wild_pattern),
		mnesia:match_object(Pattern)
		end,
	{atomic, Result} = mnesia:transaction(T),
	{ok, Result};

select({Table, Num}) when is_atom(Table), is_integer(Num) ->
	T = fun () ->
		qlc:e(qlc:q([X || X <- mnesia:table(Table)]))
	end,
	{atomic, Result} = mnesia:transaction(T),
	{ok, Result};
	
select({Table, {Field, Value}}) when is_atom(Field) ->
    T = fun () -> 
			Spec = mnesia:table_info(Table, attributes),
			Match = make_match({Table}, {Field, Value}, Spec),
			%io:format("MATCH ON: ~p~n", [Match]),
			mnesia:match_object(Match) 
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
	
 
make_match(Record, {Field, Value}, [H|T]) when Field =:= H ->
	make_match(erlang:append_element(Record, Value), {Field, Value}, T);
make_match(Record, {Field, Value}, [_|T]) ->
	make_match(erlang:append_element(Record, '_'), {Field, Value}, T);
make_match(Record, _, []) ->
	Record.

%%
run(Query) when is_tuple(Query) 
		andalso element(1, Query) =:= qlc_handle ->
	T = fun() -> qlc:e(Query) end,
	{atomic, Values} = mnesia:transaction(T),
	Values.

%%
%% Development and installation use only
%% We should assume Mnesia is running
%%
create_table(RecordType, Fields) ->
	create_table({RecordType, Fields}).
create_table(T = {Table, _}) ->
	create_tables([T]),
    ok = mnesia:wait_for_tables([Table], 10000).

create_tables([{Table, Fields}|T]) 
		when is_atom(Table), is_list(Fields) ->
	{atomic, ok} = mnesia:create_table(Table, [{disc_copies, [node()]}, 
		{attributes, Fields}]),
	create_tables(T);
create_tables([]) -> ok.
 
drop_table(Table) when is_atom(Table) ->
	drop_tables([Table]).
		
drop_tables(Tables) when is_list(Tables) -> % this should require admin privileges?
	case ewok:config({ewok, runmode}) of
	development -> 
		delete_tables(Tables, []);
	Mode -> 
		{error, {runmode, Mode}}
	end.
%	
delete_tables([H|T], Acc) ->
	{atomic, ok} = mnesia:delete_table(H),
	delete_tables(T, [H|Acc]);
delete_tables([], Acc) ->
	{ok, Acc}.

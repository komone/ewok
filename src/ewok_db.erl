-module(ewok_db).

-define(DEBUG, true).
-define(PRINT(X), io:format("~p~n", [X])).
-include("../include/ewok.hrl").

-include_lib("stdlib/include/qlc.hrl").

-compile(export_all).

start() ->
	case mnesia:system_info(use_dir) of
	true ->
		case mnesia:system_info(is_running) of
		yes -> ok; % check ewok tables?
		starting -> wait;
		stopping -> wait;
		no -> 
			mnesia:start(),
			mnesia:wait_for_tables(mnesia:system_info(tables), 10000)
		end;
	false ->
		{error, {invalid_path, mnesia:system_info(directory)}}
	end.

stop() ->
	mnesia:stop().
	
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
read(Table, Key) when is_atom(Table) ->
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
delete(Record) when is_tuple(Record) ->
	delete(element(1, Record), element(2, Record)).
delete(Table, Key) when is_atom(Table) ->
	T = fun () -> mnesia:delete({Table, Key}) end,
	{atomic, Result} = mnesia:transaction(T),
	Result.
%%
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
create_table(RecordType, Fields) ->
	create_table({RecordType, Fields}).
create_table(T = {Table, _}) ->
	create_tables([T]),
    ok = mnesia:wait_for_tables([Table], 10000).

create_tables([{Table, Fields}|T]) 
		when is_atom(Table), is_list(Fields) ->
	{atomic, ok} = 
		mnesia:create_table(Table, 
			[{disc_copies, [node()]}, 
			{attributes, Fields}]),
	create_tables(T);
create_tables([]) -> ok.
 
drop_table(Table) when is_atom(Table) ->
	drop_tables([Table]).
	
drop_tables() ->
	Tables = [X || X1 = X <- mnesia:system_info(tables), X1 =/= schema],
	drop_tables(Tables).
	
drop_tables(Tables) when is_list(Tables) -> % this should require admin privileges
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

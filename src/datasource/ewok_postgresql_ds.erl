%%  
-module(ewok_postgresql_ds, []).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("../include/ewok.hrl").

-behaviour(ewok_datasource).
-export([init/1, datasource_info/0, metadata/1, table_info/1,
	create/1, read/1, update/1, delete/1, select/1, run/1, 
	create_table/2, drop_table/1]).

%% TODO: Continuations...

%%
datasource_info() -> [
 {name, "PostgreSQL"}
]. %% ...more!

%%
init([]) ->
	ok.

%%
metadata(_Key) ->
	not_implemented.

%%
table_info(Table) when is_atom(Table) ->
	table_info({Table, all});
table_info({_Table, _Key}) ->
	not_implemented.

%%
create(Record) when is_tuple(Record) ->
	not_implemented.

%%
read({Table, _Key}) when is_atom(Table) ->
	not_implemented.

%%	
update(Record) when is_tuple(Record) ->
	not_implemented.

%%	
delete({Table, _Key}) when is_atom(Table) ->
	not_implemented;
delete(Record) when is_tuple(Record) ->
	delete({element(1, Record), element(2, Record)}).

%%
select(Table) when is_atom(Table) -> 
	not_implemented;
%%
select({Table, Num}) when is_atom(Table), is_integer(Num) ->
	not_implemented;
%%
select({_Table, {Field, _Value}}) when is_atom(Field) ->
	not_implemented;
%%
select(MatchSpec) when is_tuple(MatchSpec) ->
	not_implemented.
	

%% SQL Text as BINARY
run(Query) when is_binary(Query) ->
	not_implemented.

%%
%% Development and installation use only
%%
create_table(RecordType, Fields) 
		when is_atom(RecordType), is_list(Fields) ->
	not_implemented.
 
drop_table(Table) when is_atom(Table) ->
	not_implemented.

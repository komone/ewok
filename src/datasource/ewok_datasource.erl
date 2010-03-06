%%
-module(ewok_datasource).
-include("ewok_system.hrl").

-export([behaviour_info/1]). 

behaviour_info(callbacks) -> [
	{datasource_info, 0},
	{metadata, 1},
	{table_info, 1},
%	{start, 1},
	{create, 1},
	{read, 1},
	{update, 1},
	{delete, 1},
	{select, 1},
	{run, 1}, %% Run a native query, e.g. SQL, qlc:q(), other...
	{create_table, 2},
	{drop_table, 1}
];

behaviour_info(_) ->
    undefined. 

%%
%% NOTES
%%
%% We need to find some kind of abstracted mapping for the set of keys
%% supported by metadata/1 and table_info/1
%%
%% Continuations should ALWAYS be supported.
%%

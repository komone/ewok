%%
-module(ewok_datasource).
-include("ewok_system.hrl").

-export([behaviour_info/1]). 

%-record(db_info, {product, version, build, type, lang}).
behaviour_info(callbacks) -> [
	{init, 1},
	{terminate, 1},
	
	{info, 0},
	{info, 1},
	
	{connect, 1},
	{release, 1},
	
	{create, 1}, %% table, bucket, ...
	{drop, 1},
	
	{add, 2},
	{lookup, 2},
	{run, 1}, %% Run a native query, e.g. SQL, qlc:q(), other...
	{update, 3},
	{remove, 2}
].

%%
%% NOTES
%%
%% We need to find some kind of abstracted mapping for the set of keys
%% supported by metadata/1 and table_info/1
%%
%% Continuations should ALWAYS be supported.
%%

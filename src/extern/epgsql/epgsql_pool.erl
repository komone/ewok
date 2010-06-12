-module(epgsql_pool).

-behavior(application).
-behavior(supervisor).

-export([start/2, stop/1, init/1]).
-export([start_pool/3]).


%%
start(_Type, _Args) ->
    {ok, Pid} = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
    {ok, Pools} = application:get_env(pools),
    case catch lists:foreach(fun start_pool/1, Pools) of
	{'EXIT', Reason} -> 
		{error, Reason};
	_Other	-> 
		{ok, Pid}
    end.
%%
stop(_State) ->
    ok.

%%
init([]) ->
    {ok, {
		{simple_one_for_one, 2, 60}, [
			{pool, {pgsql_pool, start_link, []},
			permanent, 2000, supervisor, [pgsql_pool]}
		]
	}}.

%%
start_pool(Name, Size, Opts) ->
    supervisor:start_child(?MODULE, [Name, Size, Opts]).
	
%% @private
start_pool(Name) ->
    case application:get_env(Name) of
	{ok, {Size, Opts}} -> 
		start_pool(Name, Size, Opts);
	{ok, Value} -> 
		exit({invalid_pool_spec, Value});
	undefined -> 
		exit({missing_pool_spec, Name})
    end.

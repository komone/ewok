%
-module(ewok_geoip_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    File = 
		case application:get_env(geoip, datfile) of	    
		{ok, Other} -> Other;
		_ -> city
	   end,
    Process = {ewok_geoip, {ewok_egeoip, start, [File]}, 
		permanent, 5000, worker, [geoip]},
    {ok, {{one_for_one, 0, 300}, [Process]}}.	   
    

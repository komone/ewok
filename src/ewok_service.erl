-module(ewok_service).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [
	{start_link, 0},
	%{reload, 0}, ?
	{stop, 0}
];

behaviour_info(_) ->
    undefined. 

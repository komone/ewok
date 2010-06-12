-module(usp_service).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [
	{contract, 0}
];

behaviour_info(_) ->
    undefined. 

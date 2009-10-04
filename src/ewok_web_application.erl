%%
-module(ewok_web_application).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [
	{application_info, 0}
];

behaviour_info(_) ->
    undefined. 

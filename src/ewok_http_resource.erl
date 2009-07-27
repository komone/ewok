%%
-module(ewok_http_resource).

-export([behaviour_info/1]).

-interface([
	{filter/1, callback,
		"Validate an incoming request prior to execution",
		"request() -> ok | {error, Reason::string{}}"}, 
	{resource_info/0, callback,
		"Get information about this resource",
		"void -> proplist()"}
]).

behaviour_info(callbacks) -> [
	{resource_info, 0},
	{filter, 1}
];

behaviour_info(_) ->
    undefined. 

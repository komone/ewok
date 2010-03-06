%%
-module(exercise1).
-vsn("1.0").

-behaviour(ewok_http_resource).
-export([filter/1, 'GET'/2, resource_info/0]).

%% Resource Callbacks
%
resource_info() -> [
	{name, "Tutorial Exercise 1"}
].

%%
filter(_Request) -> 
	ok.

%%
'GET'(_Request, _Session) ->
	Html = <<
		"<html>",
		"<head><title>Exercise 1</title></head>",
		"<body><h1>Exercise 1</h1>",
		"<p>First things first!</p></body>",
		"</html>"
	>>,
	{ok, [{content_type, <<"text/html">>}], Html}.
	

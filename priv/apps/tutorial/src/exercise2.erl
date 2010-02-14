%%
-module(exercise2).
-vsn("1.0").

-behaviour(ewok_http_resource).
-export([filter/1, 'GET'/2, resource_info/0]).

%% Resource Callbacks
%
resource_info() -> [
	{name, "Exercise 2"}
].

%%
filter(_Request) ->
	ok.

%%
'GET'(Request, Session) ->
	Page = list_to_binary([
		<<"<html>\n">>,
		<<"<head>\n">>,
		<<"<title>Exercise 2</title>\n">>,
		<<"</head>\n">>,
		<<"<body>\n">>,
		<<"<h1>Exercise 2</h1>\n">>,
		<<"<pre>\n">>, 
		esp_html:text(Request),
		<<"</pre>\n">>, 
		<<"<pre>\n">>, 
		esp_html:text(Session),
		<<"</pre>\n">>, 
		<<"</body>\n">>,
		<<"</html>\n">>
	]),
	{ok, [{content_type, <<"text/html">>}], Page}.

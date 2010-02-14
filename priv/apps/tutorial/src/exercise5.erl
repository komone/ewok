%%
-module(exercise5).
-vsn("1.0").

%-include_lib("ewok/include/ewok.hrl").
-include_lib("ewok/include/esp.hrl").

-behavior(ewok_http_resource).
-export([filter/1, 'GET'/2, resource_info/0]).

%% Resource Callbacks
%
resource_info() -> [
	{name, "Exercise 5"}
].

%%
filter(_Request) -> ok.

%%
'GET'(Request, Session) ->
	Page = #page{
		title=[<<"Exercise 5">>],
		head=[#css{src="/tutorial.css"}],
		body=[
			#h1{body=[<<"Exercise 5">>]},
			#grid{caption=[<<"Request Object API">>], 
				header=[<<"Function">>, <<"Value">>],
				body=[
					[<<"socket()">>, esp_html:text(Request:socket())],
					[<<"timeout()">>, esp_html:text(Request:timeout())],
					[<<"remote_ip()">>, esp_html:text(Request:remote_ip())],
					[<<"method()">>, esp_html:text(Request:method())],
					[<<"url()">>, esp_html:text(Request:url())],
					[<<"path()">>, esp_html:text(Request:path())],
					[<<"version()">>, esp_html:text(Request:version())],
					[<<"realm()">>, esp_html:text(Request:realm())],
					[<<"cookie()">>, esp_html:text(Request:cookie())],
					[<<"headers()">>, esp_html:text(Request:headers())],
					[<<"header(user_agent)">>, esp_html:text(Request:header(user_agent))],
					[<<"parameters()">>, esp_html:text(Request:parameters())],
					[<<"parameter(<<\"foo\">>)">>, esp_html:text(Request:parameter(<<"foo">>))],
					[<<"parameter(\"foo\")">>, esp_html:text(Request:parameter("foo"))]
				]},
			#br{},
			#grid{caption=[<<"Session Object API">>], 
				header=[<<"Function">>, <<"Value">>], 
				body=make_session_api(Session)}
		]},
	esp:render(Page).

%%
make_session_api(Session) ->
	Session:save(test_data, <<"Some example test data">>),
	F = fun(X) -> [list_to_binary([esp_html:text(X), <<"()">>]), esp_html:text(Session:X())] end,
	[F(X) || {X = X1, 1} <- ewok_session_obj:module_info(exports), X1 =/= reset, X1 =/= module_info].
	



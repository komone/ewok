%%
-module(exercise3).
-vsn("1.0").

-include_lib("ewok/include/esp.hrl").

-behaviour(ewok_http_resource).
-export([filter/1, 'GET'/2, resource_info/0]).

%% Resource Callbacks
%
resource_info() -> [
	{name, "Exercise 3"}
].

%%
filter(_Request) ->
	ok.

%%
'GET'(Request, Session) ->
	Page = [
		#html{body=[
			#head{body=[
				#title{body=[<<"Exercise 3">>]}
			]},
			#body{body=[
				#h1{body=[<<"Exercise 3">>]},
				#pre{body=[esp_html:text(Request)]},
				#pre{body=[esp_html:text(Session)]}
			]}
		]}
	],
	{ok, Markup} = esp:render_page([Page], ?MODULE, Request, Session),
	{ok, [{content_type, <<"text/html">>}], Markup}.

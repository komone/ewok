%%
-module(exercise4).
-vsn("1.0").

-include_lib("ewok/include/esp.hrl").

-behaviour(ewok_http_resource).
-export([filter/1, 'GET'/2, resource_info/0]).

%% Resource Callbacks
%
resource_info() -> [
	{name, "Exercise 4"}
].

%%
filter(Request) ->
	case Request:method() of
	'GET' -> ok;
	_ -> method_not_supported
	end.

%%
'GET'(Request, Session) ->
	Page = 
		#page{
			title=[<<"Exercise 4">>],
			head=[#css{src="/tutorial.css"}],
			body=[
				#h1{body=[<<"Exercise 4">>]},
				#pre{body=[esp_html:text(Request)]},
				#pre{body=[esp_html:text(Session)]}
			]
		},
	esp:render(Page).

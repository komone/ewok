-module(ewok_print_handler).
-include("../include/ewok.hrl").
-include("../include/esp.hrl").

-behavior(ewok_http_resource).
-export([filter/1, resource_info/0]).

-export(['GET'/2, 'POST'/2]).

filter(_Request) -> ok.
resource_info() -> [].

'GET'(Request, Session) ->
	Spec = [
		{title, <<"ESP Ajax Test">>},
		{menu, [#p{body=[<<"Ewok Server Pages Tests">>]}]},
		{content, [
			#h1{body=[<<"Ajax Form">>]},
			esp_html:inplace([
				#inplace{label= <<"Username: ">>, id= <<"username">>, value= <<"Merchant Republic">>},
				#inplace{label= <<"Activation Key: ">>, id= <<"activation">>, value= <<"Enter Key Here">>}
			]),
			#p{body=[
				<<"from ">>,
				esp_html:datepicker("datefrom"),
				<<" to ">>,
				esp_html:datepicker("dateto")
			]}
		]}
	],
	ewok_web:render(Request, Session, Spec).
	
'POST'(Request, _Session) ->
	Params = {
		Request:method(), 
		Request:path(), 
		Request:header(content_type),
		Request:header(content_length), 
		Request:parameters(),
		Request:content()
	},
	
	%%io:format(user, "REQ~n~p~n", [Request]),
	io:format(user, "PARAMS~n~p~n", [Params]),
	case Request:header(content_type) of
	<<"application/json">> -> 
		{ok, [{content_type, <<"application/json">>}], [<<"{date: ">>, ewok_http:date(), <<"}">>]};
	_ ->
		{ok, [{content_type, <<"text/html">>}], Request:parameter(<<"value">>)}
	end.


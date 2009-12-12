-module(ewok_world).
-include("ewok.hrl").
-include("esp.hrl").

-behaviour(ewok_http_resource).
-export([filter/1, resource_info/0]).

-export(['GET'/2, 'POST'/2]).

filter(_Request) -> ok.
resource_info() -> [].

'GET'(Request, _Session) ->
	ewok_log:info([Request]),
	Page = #page{title= <<"Simulacity">>,head=[],
		body=[#h1{body=[<<"Welcome to Simulacity">>]}]},
	esp:render(Page). 
	
'POST'(Request, _Session) ->
	ewok_log:info([Request]),
	case Request:header(expect) of
	<<"100-continue">> -> 
		Response = {response, 100, [], [], false},	
		{ok, _HttpResponse, _BytesSent} = ewok_response:reply(Request, Response),
		ewok_log:info({content, Request:content()}),
		{ok, Bin} = file:read_file("login-out.xml"),
		{ok, [{content_type, <<"application/xml">>}], [Bin]};
	_ ->
		{ok, [], []}
	end.
	

-module(ewok_mock).

-compile(export_all).

request() ->
	Req = ewok_request_obj:new(undefined, 30000,
		'GET', <<"/mock/test/file.html?wsdl">>, <<"1.1">>, [], 99),
	Req:set_realm(ewok),
	Req.


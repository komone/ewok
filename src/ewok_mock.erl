-module(ewok_mock).

-compile(export_all).

%-module(ewok_request_obj, [Socket, Timeout, Method, Url, Version, Headers, MaxHeaders]).

request() ->
	Req = ewok_request_obj:new(
		undefined,
		30000,
		'GET',
		<<"/mock/test/file.html?foo=bar">>,
		<<"1.1">>,
		[],
		99
	),
	Req:set_realm(ewok),
	Req.

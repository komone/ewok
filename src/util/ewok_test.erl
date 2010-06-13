%%
-module(ewok_test).
-compile(export_all).
-include("../ewok_geoip.hrl").

start() ->
	io:format("~p~n", [test]),
	case io:format("~p~n", [test]) of
	ok ->
		io:format("~p~n", [test]);
	_ ->
		[io:format("~p~n", [X]) || X <- [1,2,3]]
	end.

geoip_valid() ->
	TestIp = "207.145.216.106",
    R = ewok_geoip:lookup(TestIp),
    #geoip{country_code = <<"US">>,
	   country_code3 = <<"USA">>,
	   country_name = <<"United States">>,
	   region = <<"CA">>,
           _ = _} = R,
    %% This is the test IP that MaxMind uses
    R1 = ewok_geoip:lookup("24.24.24.24"),
    #geoip{country_code = <<"US">>,
	   country_code3 = <<"USA">>,
	   country_name = <<"United States">>,
	   region = <<"NY">>,
           _ = _} = R1,
	ewok_geoip:lookup("24.217.133.98").

geoip_bench() ->
    geoip_bench(1000).
geoip_bench(Count) ->
    SampleIPs = [
		"63.224.214.117",
		"144.139.80.91",
		"88.233.53.82",
		"85.250.32.5",
		"220.189.211.182",
		"211.112.118.99",
		"84.94.205.244",
		"61.16.226.206",
		"64.180.1.78",
		"138.217.4.11"
	],
    StartParse = now(),
    benchcall(fun () -> [ewok_geoip:lookup(X) || X <- SampleIPs] end, Count),
    EndParse = now(),
    {geoip_lookup, Count * length(SampleIPs), pytime(EndParse) - pytime(StartParse)}.

benchcall(Fun, 1) ->
    Fun();
benchcall(Fun, Times) ->
    Fun(),
    benchcall(Fun, Times - 1).

pytime({MegaSecs, Secs, MicroSecs}) ->
    (1.0e+6 * MegaSecs) + Secs + (1.0e-6 * MicroSecs).

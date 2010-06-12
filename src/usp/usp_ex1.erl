%%
-module(usp_ex1).
-include("usp.hrl").

-behaviour(usp_service).
-export([contract/0]).

-export([stock_quote/1]).

%-record(usp_type, {id, term, contraints = []}).
%-record(usp_service, {call, in = [], out = [], start, next}).

contract() -> [
	#usp_type{
		id = symbol,
		term = binary,
		contraints = [{regex, <<"^[A-Z]+$">>}, {length, 3, 4}]
	},
	#usp_operation{
		call = stock_quote, 
		in = [symbol],
		out = integer,
		start = any,
		next = any
	}
].

stock_quote(<<Symbol/binary>>) ->
	lookup(Symbol).

lookup(Symbol) ->
	Ticker = [
		{<<"AAPL">>, 32.0},
		{<<"CISC">>, 53.9},
		{<<"IBM">>, 3.79},
		{<<"ORCL">>, 24.88}
	],
	proplists:get_value(Symbol, Ticker, 0).


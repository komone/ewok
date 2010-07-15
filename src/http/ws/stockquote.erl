-module(stockquote).
-include("ewok.hrl").

-behaviour(ewok_http_resource).
-export([filter/1, resource_info/0, 'GET'/2, 'POST'/2]).

resource_info() ->
	[].
	
filter(_Request) ->
	ok.
%	{delegate, ewok_webservice, [?MODULE]}.
	
'GET'(Request, Session) ->
	handle_request(Request, Session).
	
'POST'(Request, Session) ->	
	handle_request(Request, Session).

handle_request(Request, _Session) ->
	?TTY(Request),	
	[Query] = ewok_soap:decode(Request#http_request.content),
%	?TTY(Query),	
	{Ticker, Quote} = get_stock_quote(Query),
	Response = ewok_soap:encode({trade_price, [{symbol, Ticker}, {price, Quote}]}),
%	?TTY(Response),
	{ok, [], Response}.
	
get_stock_quote(Ticker) ->
	lists:keyfind(Ticker, 1, [
		{<<"AAPL">>, 10.1},
		{<<"IBM">>, 20.4},
		{<<"MSFT">>, 110.3}
	]).


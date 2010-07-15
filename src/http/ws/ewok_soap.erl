-module(ewok_soap).
-include("ewok.hrl").
-compile(export_all).

-define(XMLNS_SOAP_ENVELOPE, <<"http://schemas.xmlsoap.org/soap/envelope">>).

-define(KEYS, [soap, tns]).

examples() -> [
"<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:stoc=\"http://localhost:8080/stockquote.xsd\">
   <soapenv:Header/>
   <soapenv:Body>
	<stoc:TradePriceRequest>
		<stoc:tickerSymbol>AAPL</stoc:tickerSymbol>
	</stoc:TradePriceRequest>
   </soapenv:Body>
</soapenv:Envelope>",

"<?xml version=\"1.0\"?>
<soapenv:Envelope xmlns:soapenv=\"http://schemas.xmlsoap.org/soap/envelope/\" xmlns:stoc=\"http://localhost:8080/stockquote.xsd\">
	<soapenv:'Header'/>
	<soapenv:'Body'>
		<stoc:TradePrice>
			<stoc:price>10.1</stoc:price>
		</stoc:TradePrice>
	</soapenv:'Body'>
</soapenv:Envelope>"
].

encode({Action, Params}) ->
%	?TTY(Params),
%	List = [{{<<"tns">>, X}, [], [ewok_text:encode(Y)]} || {X, Y} <- Params],
%	?TTY(List),
	ewok_xml:encode([
		{{soap,<<"Envelope">>}, [
			{{xmlns, soap}, ?XMLNS_SOAP_ENVELOPE},
			{{xmlns, tns}, <<"http://localhost:8080/stockquote.xsd">>}
		], [
			{{soap, <<"Header">>},[],[]}, 
			{{soap,<<"Body">>}, [], [
				{{tns, Action}, [], 
				[{{tns, X}, [], [ewok_text:encode(Y)]} || {X, Y} <- Params]
			}]
		}]
	}]).

decode(Bin) ->
	Xml = ewok_xml:decode(Bin),
	ewok_xml:xpath(<<"/soapenv:Envelope/soapenv:Body/stoc:TradePriceRequest/stoc:tickerSymbol/text()">>, Xml).


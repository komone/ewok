-module(usp_ws).
-include("usp.hrl").

-behaviour(ewok_http_resource).
-export([filter/1, resource_info/0, 'GET'/2]).

-compile(export_all).

-define(WSDL_SCHEMA, <<"http://schemas.xmlsoap.org/wsdl/">>).
-define(SOAP_SCHEMA, <<"http://schemas.xmlsoap.org/wsdl/soap/">>).
-define(SOAP_HTTP_SCHEMA, <<"http://schemas.xmlsoap.org/soap/http">>).
-define(XML_SCHEMA, <<"http://www.w3.org/2001/XMLSchema">>).

-xsd([{stockquote, string, [0,1]}]).

resource_info() ->
	[].

filter(_Request) ->
	ok.
	
'GET'(Request, _Session) ->
	case Request:parameter(<<"wsdl">>) of
	true ->
		{ok, [], wsdl(Request:path(), ?MODULE)};
	false ->
		_Message = ewok_xml:decode(Request:body()),
	
		Body = encode_response({result, [], ok}),
		{ok, [], Body}
	end.
	
%% example contract callback from ex1	
contract() -> [
	#usp_type{
		id = symbol,
		term = binary,
		contraints = [{size, 4}]
	},
	#usp_operation{
		call = stock_quote, 
		in = [symbol],
		out = integer,
		start = any,
		next = any
	}
].

%-module(ewok_request_obj, [Socket, Timeout, Method, Url, Version, Headers, MaxHeaders]).

test() ->
	Request = ewok_request_obj:new(undefined, 3000, 'GET', <<"/service/stockquote?wsdl">>, {1,1},
		[{<<"Host">>, <<"http://simulacity.com:8080">>}], 100),
	file:write_file("test.wsdl", wsdl(Request, usp_ex1)).
example() -> "
<Quote 
		xmlns=\"http://www.xignite.com/services/\"
		xmlns:xsd=\"http://www.w3.org/2001/XMLSchema\" 
		xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\">
	
	<Outcome>RegistrationError</Outcome> 
	<Message>
		XigniteQuotes: Maximum number of unregistered requests exceeded. 
		Consider registering or subscribing to expand usage. 
		Your request was authenticated using  your IP address (24.217.133.98). 
		Please visit http://www.xignite.com/RegistrationHelp.aspx for more 
		information.
	</Message> 
	<Identity>IP</Identity> 
	<Delay>0</Delay> 
	<Open>0</Open> 
	<High>0</High> 
	<Low>0</Low> 
	<Last>0</Last> 
	<Volume>0</Volume> 
	<Change>0</Change> 
	<PercentChange>0</PercentChange> 
</Quote>
".

wsdl(Request, Handler) ->
	Name = xmlname(Handler),
	Host = Request:header(host),
	Endpoint = list_to_binary([Host, Request:path()]),
	_Contract = contract(), %% Handler:contract(),
	Types = [{types, [], []}],
	Messages = messages(Handler),
	Cruft = [
	{portType, [{name, <<Name/binary, "PortType">>}], [
		{operation, [{name, Name}], [
			{input, [{message, <<"tns:", Name/binary, "Input">>}], []},
			{output, [{message, <<"tns:", Name/binary, "Output">>}], []}
		]}
	]},
	{binding, [
		{name, <<Name/binary, "SoapBinding">>}, 
		{type, <<"tns:", Name/binary, "PortType">>}
	], [
		{<<"soap:binding">>, [{style, <<"document">>}, {transport, ?SOAP_HTTP_SCHEMA}], []},
		{operation, [{name, Name}], [
			{<<"soap:operation">>, [{<<"soapAction">>, Endpoint}], [
				{input, [], [{<<"soap:body">>, [{use, <<"literal">>}], []}]},
				{output, [], [{<<"soap:body">>, [{use, <<"literal">>}], []}]}
			]}
		]}
	]},
	{service, [{name, <<Name/binary, "Service">>}], [
		{documentation, [], [<<Name/binary, " Service">>]},
		{port, [
			{name, <<Name/binary, "Port">>}, 
			{binding, <<Name/binary, "SoapBinding">>}
		], [
			{<<"soap:address">>, [{location, Endpoint}], []}
		]}
	]}],
	Body = lists:append([Types, Messages, Cruft]),
	WSDL = [ 
		{definitions, [
			{name, Name},
			{<<"targetNamespace">>, <<Endpoint/binary, ".wsdl">>},
			{<<"xmlns:tns">>, <<Endpoint/binary, ".wsdl">>},
			{<<"xmlns:xsdx">>, <<Endpoint/binary, ".xsd">>},
			{<<"xmlns">>, ?WSDL_SCHEMA},	
			{<<"xmlns:xs">>, ?XML_SCHEMA},	
			{<<"xmlns:soap">>, ?SOAP_SCHEMA}
	], Body}],
	ewok_xml:encode(WSDL).

%
types(Handler) ->
	Functions = Handler:module_info(exports),
	types(Functions, []).
types([{module_info, _Arity}|T], Acc) ->
	types(T, Acc);
types([H|T], Acc) ->
	types(T, [type(H)|Acc]);
types([], Acc) ->
	lists:reverse(Acc).
	
type({F, Arity}) ->
	Name = xmlname(F),
	{complexType, [{name, <<Name/binary>>}], [
		{sequence, [], string(F, [], Arity)}
	]}.
	
string(F, Acc, Count) when Count > 0 ->
	Name = xmlname(F),
	E = {element, [
		{minOccurs, <<"0">>},
		{maxOccurs, <<"1">>},
		{name, <<Name/binary, (integer_to_list(Count))/binary>>},
		{type, <<"xs:string">>}
	], []},
	string(F, [E|Acc], Count - 1);
string(_, Acc, _) ->
	lists:reverse(Acc).

%
messages(Handler) ->
	Functions = Handler:module_info(exports),
	messages(Functions, []).
%	
messages([{module_info, _Arity}|T], Acc) ->
	messages(T, Acc);
messages([H|T], Acc) ->
	messages(T, [ 
		message(H, <<"Output">>, <<"">>), 
		message(H, <<"Input">>, <<"Request">>)
		|Acc]);
messages([], Acc) ->
	lists:reverse(Acc).

message({F, _Arity}, Type, Suffix) ->
	Name = xmlname(F),
	{message, [{name, <<Name/binary, Type/binary>>}], [
		{part, [{name, <<"body">>}, {element, <<"xsdx:", Name/binary, Suffix/binary>>}], []}
	]}.

	
encode_response(Result) ->
	%% ..,
	ewok_xml:encode(Result).

xmlname(Atom) when is_atom(Atom) ->
	xmlname(atom_to_binary(Atom, utf8), <<>>).
xmlname(<<X, Rest/binary>>, <<>>) ->
	xmlname(Rest, <<(ewok_text:to_upper(X))/binary>>);
xmlname(<<$_, X, Rest/binary>>, Acc) ->
	xmlname(Rest, <<Acc/binary, (ewok_text:to_upper(X))/binary>>);
xmlname(<<X, Rest/binary>>, Acc) ->
	xmlname(Rest, <<Acc/binary, X>>);
xmlname(<<>>, Acc) ->
	Acc.
	

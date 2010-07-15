-module(ewok_wsdl).
-include("ewok.hrl").

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

-compile(export_all).

-define(XMLNS_XSD, <<"http://www.w3.org/2001/XMLSchema">>).
-define(XMLNS_WSDL, <<"http://schemas.xmlsoap.org/wsdl/">>).
-define(XMLNS_WSDL_SOAP, <<"http://schemas.xmlsoap.org/wsdl/soap/">>).
-define(XMLNS_SOAP_HTTP, <<"http://schemas.xmlsoap.org/soap/http">>).

-define(KEYS, [xmlns, targetNamespace, 
	types, schema, element,
	message, part,
	portType, operation, input, output, fault, binding,
	service, address,
	soap, soapAction, string, float]).

decode(Bin) ->	
	Xml = ewok_xml:decode(Bin),
	decode_wsdl(Xml).
	
encode(T = #wsdl{id = ID, path = Path}) ->
	?TTY(T),
	Name = encode_name(ID),
	Size = size(Name),
	Operations = encode_operations(T),
	Xml = [{definitions, [
			{name, Name},
			{xmlns, ?XMLNS_WSDL},
			{{xmlns, xsd}, ?XMLNS_XSD},
			{{xmlns, soap}, ?XMLNS_WSDL_SOAP}
		], [
			{types, [], []},
			{message, [], []},
			{portType, [], []},
			{binding, [
				{name, <<Name:Size/binary, "SoapBinding">>},
				{type, <<"tns:", Name:Size/binary, "PortType">>}
			], [{{soap, binding}, [
					{style, document},
					{transport, ?XMLNS_SOAP_HTTP}
				], []} 
				| Operations
			]},
			{service, [{name, <<Name:Size/binary, "Service">>}], [ 
				{port, [
					{name, <<Name:Size/binary, "Port">>}, 
					{binding, <<"tns:", Name:Size/binary, "SoapBinding">>}
				], [{{soap, address}, [
						{location, Path}
					], [
					]}
				]}
			]}
		]}
	],
	ewok_xml:format(ewok_xml:encode(Xml)).
	
encode_operations(Wsdl = #wsdl{id = ID, ops = any}) ->
	Exported = [F || F = {X, _} <- ID:module_info(exports), X =/= module_info, X =/= filter],
	encode_operations(Wsdl#wsdl{ops = Exported});
encode_operations(#wsdl{ops = Ops}) ->
	encode_operations(Ops, []).
	
encode_operations([{Fun, _Arity}|T], Acc) ->
	Name = encode_name(Fun),
	Op = 
		{operation, [{name, Name}], [
			{{soap, operation}, [
				{soapAction, encode_name(Name)}
			], []},
			{input, [], [{{soap, body}, [{use, literal}], []} ]},
			{output, [], [ {{soap, body}, [{use, literal}], []} ]}
		]},
	encode_operations(T, [Op|Acc]);
encode_operations([], Acc) ->
	lists:reverse(Acc).


decode_wsdl({xml, _, [{definitions, Attrs, Elements}]}) ->
	Name = proplists:get_value(name, Attrs),
	E = get_element([service, port, {soap, address}], Elements),
	Url = get_attribute(location, E),
	#wsdl{id=Name, path=Url}.

get_element([H|T], Body) ->
	case match_element(H, Body) of
	undefined ->
		undefined;
	Element when T =:= [] ->
		Element;
	{H, _, Body0} ->
		get_element(T, Body0)
	end.
	
match_element(X, [E = {X, _, _}|_]) ->
	E;
match_element(X, [_|T]) ->
	match_element(X, T);
match_element(_, []) ->
	undefined.

get_attribute(Name, {_, Attrs, _}) ->
	proplists:get_value(Name, Attrs).

generate(Mod) when is_atom(Mod) ->
	Name = encode_name(Mod),
	Funs = [X || X = {Y, _} <- Mod:module_info(exports), Y =/= module_info],
	Ops = [{encode_name(X), generate_params(Y)} || {X, Y} <- Funs],
	Wsdl = #wsdl{id = Name, path = <<"/", Name/binary>>, ops = Ops},
	encode(Wsdl).

generate_params(X) when is_integer(X), X > 0 ->
	generate_params(X, []).
generate_params(X, Acc) when X > 0 ->
	X1 = X - 1,
	Bin = ewok_text:encode(X1),
	generate_params(X1, [<<"arg", Bin/binary>> | Acc]);
generate_params(_, Acc) ->
	Acc.

encode_name(X) when is_atom(X) ->
	encode_name(atom_to_binary(X, utf8));
encode_name(<<X, Rest/binary>>) ->
	encode_name(Rest, [ewok_text:to_upper(X)]).
encode_name(<<$_, X, Rest/binary>>, Acc) ->
	encode_name(Rest, [ewok_text:to_upper(X) | Acc]);
encode_name(<<X, Rest/binary>>, Acc) ->
	encode_name(Rest, [X | Acc]);
encode_name(<<>>, Acc) ->
	list_to_binary(lists:reverse(Acc)).

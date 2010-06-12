-module(ewok_http_codec).

-include("ewok.hrl").
-include("ewok_system.hrl").

-define(SP, <<" ">>).
-define(CRLF, <<"\r\n">>).
-define(EOH, <<"\r\n\r\n">>).

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

encode({Status, Headers, Body}) ->	
	Response = [
		<<"HTTP/1.1 ">>,
		ewok_text:encode(ewok_http:status_code(Status)), ?SP,
		ewok_http:status_message(Status), ?CRLF,
		encode_headers([{server, ?SERVER_ID}, {date, ewok_http:date()} | Headers], []),
		?CRLF,
		Body
	],
	list_to_binary(Response).

encode_headers([{K, V}|T], Acc) ->
	encode_headers(T, [[ewok_http:header(K), <<":">>, ?SP, V, ?CRLF] | Acc]);
encode_headers([], Acc) ->
	list_to_binary(lists:reverse(Acc)).

%%
decode(Bin) ->
	[Head | Body] = ewok_text:split(Bin, ?EOH, 2),
	[Command | Lines] = ewok_text:split(Head, ?CRLF),
	[Method, FullPath, <<"HTTP/", Version/binary>>] = ewok_text:split(Command, ?SP),
	[Path | Query] = ewok_text:split(FullPath, <<"\\?">>, 2),
	Headers = decode_headers(Lines, []),
	Params = decode_params(ewok_text:split(Query, <<"&">>), []),
	#request{
		method = Method,
		version = Version,
		path = Path,
		headers = Headers,
		params = Params,
		content = Body
	}.
%%
decode_headers([H|T], Acc) ->
	[Name, Value] = ewok_text:split(H, <<":">>, 2),
	decode_headers(T, [{ewok_text:trim(Name), ewok_text:trim(Value)} | Acc]);
decode_headers([], Acc) ->
	lists:reverse(Acc).
%%
decode_params([H|T], Acc) ->
	[K | V] = ewok_text:split(H, <<"=">>, 2),
	decode_params(T, [{K, list_to_binary(V)} | Acc]);
decode_params([], Acc) ->
	lists:reverse(Acc).

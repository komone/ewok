%% Copyright 2010 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(ewok_http_client).
-include("ewok.hrl").

-export([head/1, head/2, get/1, get/2, post/3, request/4]).

-define(USER_AGENT, <<"Mozilla/5.0 (Windows NT 5.1; en-US) Ewok/1.0.0">>).
-define(PROTOCOL, <<"HTTP/1.1">>).
-define(NEWLINE, <<"\r\n">>).

-define(SOCKET_TIMEOUT, 10000).

-compile(export_all).

%%
head(Url) ->
	head(Url, []).
head(Url, Options) ->
	request('HEAD', Url, Options, []).

%%
get(Url) ->
	get(Url, []).
get(Url, Options) ->
	request('GET', Url, Options, []).
	
%%
post(Url, Headers, Params = [{_, _}|_]) ->
	post(Url, Headers, querystring(Params, []));
post(Url, Headers, Body) when is_binary(Body) ->
	request('POST', Url, Headers, Body).

querystring([H], Acc) ->
	list_to_binary(lists:reverse([queryparam(H) | Acc]));
querystring([H|T], Acc) ->
	querystring(T, [<<(queryparam(H))/binary, $&>> | Acc]);
querystring([], []) ->
	<<>>.
	
queryparam({K, V}) ->
	Key = ewok_http:url_encode(K),
	Value = ewok_http:url_encode(V),
	<<Key/binary, $=, Value/binary>>.
	

% GET / HTTP/1.1
% Host: localhost:8080
% Connection: keep-alive
% User-Agent: Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US) AppleWebKit/532.5 (KHTML, like Gecko) Chrome/4.0.249.78 Safari/532.5
% Accept: application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5
% Accept-Encoding: gzip,deflate,sdch
% Accept-Language: en-US,en;q=0.8
% Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3
%% TODO: Headers, Body
request(Method, Url, _Headers, _Body) when is_atom(Method) ->
	{Transport, Host, Port, Resource} = parse_url(Url),
	RequestLine = <<(method(Method))/binary, " ", Resource/binary, " ", ?PROTOCOL/binary, "\r\n">>,
	Request = list_to_binary([RequestLine, headers(Host, Port), ?NEWLINE]),
    {ok, Socket} = ewok_socket:connect(Transport, Host, Port, [binary, {active, false}, {packet, 0}]),
	ewok_socket:send({Transport, Socket}, Request),
	response({Transport, Socket}, Method).
	
%%
response(Socket, Method) ->
    {ok, Packet} = ewok_socket:recv(Socket, 0, ?SOCKET_TIMEOUT),
	{Status, Headers, Data} = parse_response(Packet),
%	?TTY({Status, Headers}),
	case Method of
	'HEAD' ->
		Content = [Data];
	_ ->
		{ok, Content} = read_content(Socket, Headers, Data)
	end,
    ewok_socket:close(Socket),
    {Status, Headers, list_to_binary(Content)}.

%%
parse_response(Packet) ->
	[StatusLine, Rest] = ewok_util:split(Packet, ?NEWLINE, 2),
	[_Protocol, StatusCode, _Message] = ewok_util:split(StatusLine, <<$ >>, 3),
	Status = ewok_http:status(list_to_integer(binary_to_list(StatusCode))),
	case ewok_util:split(Rest, <<"\r\n\r\n">>, 2) of
	[HeaderLines, Body] -> 
		ok;
	[HeaderLines] -> 
		Body = <<>>
	end,
	Headers = [parse_header(X) || X <- ewok_util:split(HeaderLines, ?NEWLINE)],
	{Status, Headers, Body}.

%%
parse_header(Header) ->
	[K, V] = ewok_util:split(Header, <<$:>>, 2),
	{ewok_util:trim(K), ewok_util:trim(V)}.

%%
read_content(Socket, Headers, Content) ->
	ContentLength = proplists:get_value(<<"Content-Length">>, Headers),
	TransferEncoding = proplists:get_value(<<"Transfer-Encoding">>, Headers),
	case {ContentLength, TransferEncoding} of
	{undefined, undefined} ->
		{ok, [Content]};
	{undefined, <<"chunked">>} ->
		read_chunked_content(Socket, Content, []);
	{X, undefined} ->
		Length = list_to_integer(binary_to_list(X)),
		case Length - size(Content) of
		0 ->
			{ok, [Content]};
		Y when Y > 0 ->
			{ok, Rest} = ewok_socket:recv(Socket, Y, ?SOCKET_TIMEOUT),
			{ok, [Content, Rest]}
		end;
	%% TODO: gzip, deflate, compressed...
	{_X, Other} when is_binary(Other) ->
		{error, {unsupported_encoding, Other}}
	end.
	
%%
read_chunked_content(_Socket, ?NEWLINE, Acc) ->
	{ok, lists:reverse(Acc)};
read_chunked_content(Socket, Packet, Acc) ->
%	?TTY(ewok_util:split(Packet, ?NEWLINE, 3)),
	[ChunkSize|Part] = ewok_util:split(Packet, ?NEWLINE, 3),
	Chunk = list_to_binary(Part),
	[Hex|_] = ewok_util:split(ChunkSize, <<$;>>, 2),
	Size = ewok_util:hexint(Hex),
%	?TTY({Size, size(Chunk)}),
	case {Size, size(Chunk)} of 
	{0, _} ->
		{ok, lists:reverse([Chunk|Acc])};	
	{X, 0} ->
		{ok, Rest} = ewok_socket:recv(Socket, X, ?SOCKET_TIMEOUT),
		{ok, lists:reverse([Rest, Chunk|Acc])};	
	{X, X} ->
		{ok, lists:reverse([Chunk|Acc])};
	{X, Y} when X > Y ->
		{ok, Rest} = ewok_socket:recv(Socket, (X - Y), ?SOCKET_TIMEOUT),
		{ok, Next} = ewok_socket:recv(Socket, 0, ?SOCKET_TIMEOUT),
%		?TTY({continue, X, Y, size(Rest), Next}),
		read_chunked_content(Socket, Next, [Rest, Chunk|Acc]);
	{X, Y} when X < Y ->
%		?TTY({continue, X, Y}),
		<<Rest:Size/binary, Next/binary>> = Chunk,
		read_chunked_content(Socket, Next, [Rest|Acc])
	end.

%%
parse_url(Url) ->
	case ewok_util:split(Url, <<"//|(:)|(/.*)">>) of
	[Protocol, <<$:>>, Host] ->
		Port = port(Protocol, <<>>),
		Resource = <<$/>>;
	[Protocol, <<$:>>, Host, Resource] ->
		Port = port(Protocol, <<>>);
	[Protocol, <<$:>>, Host, <<$:>>, P] ->
		Port = port(Protocol, P),
		Resource = <<$/>>;
	[Protocol, <<$:>>, Host, <<$:>>, P, Resource] ->
		Port = port(Protocol, P),
		ok
	end,
	{transport(Protocol), Host, Port, Resource}.

%%
method('HEAD') -> <<"HEAD">>;
method('GET')  -> <<"GET">>;
method('POST') -> <<"POST">>.

%%
transport(<<"http">>)  -> gen_tcp;
transport(<<"https">>) -> ssl.

%%
port(<<"http">>, <<>>)  -> <<"80">>;
port(<<"https">>, <<>>) -> <<"443">>;
port(_, X) when is_binary(X) -> X.

%%
headers(Host, Port) -> [
	<<"Host: ", Host/binary, $:, Port/binary, "\r\n">>,
	<<"Connection: keep-alive\r\n">>,
	<<"User-Agent: ">>, ?USER_AGENT, <<"\r\n">>,
	<<"Accept: application/xml,application/xhtml+xml,text/html;q=0.9,text/plain;q=0.8,image/png,*/*;q=0.5\r\n">>,
	%<<"Accept-Encoding: gzip,deflate\r\n">>,
	<<"Accept-Language: en-US,en;q=0.8\r\n">>,
	<<"Accept-Charset: ISO-8859-1,utf-8;q=0.7,*;q=0.3\r\n">>
].

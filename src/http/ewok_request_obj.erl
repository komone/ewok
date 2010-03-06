%% Copyright 2009 Steve Davis <steve@simulacity.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ewok_request_obj, [Socket, Timeout, Method, Url, Version, Headers, MaxHeaders]).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include_lib("kernel/include/file.hrl").

-export([get_range/0, content_length/0, socket/0, timeout/0, 
	max_headers/0, remote_ip/0, version/0, url/0, headers/0, 
	header/1, content/0, path/0, method/0, realm/0, set_realm/1]).
-export([recv/1, recv/2, should_close/0]).
-export([cookie/0, cookie/1, parameter/1, parameters/0]).
-export([reset/0, websocket/0]).

%% expose for test only
-compile(export_all).

%%
-define(REALM, www_request_realm).
-define(PATH, www_request_path).
-define(REMOTE_IP, www_request_ip).
-define(COOKIE, www_request_cookie).
-define(QUERY, www_request_query).
-define(CONTENT_LENGTH, www_request_length).
-define(CONTENT, www_request_body).
-define(WEB_SOCKET, www_web_socket).
-define(CACHE, [?REALM, ?PATH, ?REMOTE_IP, ?COOKIE, ?QUERY, ?CONTENT_LENGTH, ?CONTENT]).

% 10 second default idle timeout
-define(IDLE_TIMEOUT, 10000).

% Maximum recv_body() length of 1MB
-define(MAX_RECV_BODY, (1024 * 1024)).

%% NOTE!!! This is required before continuing with Keep-Alive however this
%% call also breaks the write-once contract with the Process Dictionary, 
%% and thus may invalidate the entire approach of using 'pseudo-objects'
%% to improve the development API... see the alternative idea of an 
%% ewok_http_connection handler that is a gen_event process.
reset() ->
    [erase(K) || K <- ?CACHE].
	
socket()      -> Socket.
timeout()     -> Timeout.
version()     -> Version.
url()         -> Url.
method()      -> Method.
headers()     -> Headers.
max_headers() -> MaxHeaders.

%%
websocket() ->
	case erlang:get(?WEB_SOCKET) of
	undefined -> erlang:put(?WEB_SOCKET, true);
	Value -> Value
	end.
	
%%
remote_ip() ->
	case erlang:get(?REMOTE_IP) of
	undefined ->
		IP = ewok_http:get_remote_ip(Socket, header(<<"X-Forwarded-For">>)),
		erlang:put(?REMOTE_IP, IP),
		IP;
	IP -> IP
	end.

%% @private
content_length() ->
	case erlang:get(?CONTENT_LENGTH) of
	undefined ->
		Length = 
			case header(transfer_encoding) of
			undefined ->
				case header(content_length) of
				undefined -> 0;
				Value -> list_to_integer(binary_to_list(Value))
				end;
			<<"chunked">> -> 
				chunked;			
			Unknown ->
				{unknown_transfer_encoding, Unknown}
			end,
		erlang:put(?CONTENT_LENGTH, Length),
		Length;
	Length -> Length
	end.

%%
content() -> 
    case header(expect) of
	<<"100-continue">> -> ewok_response:continue(THIS);
	_ -> ok %% expectation failed...
    end,
	case erlang:get(?CONTENT) of
	undefined -> 
		Body = 
			case content_length() of
			undefined -> <<>>;
			X when X =:= 0 -> <<>>;
			X when is_integer(X) -> recv(X);
			chunked ->
				read_chunked_body(?MAX_RECV_BODY, []);
			_ -> {error, not_implemented}
			end,
		% NOTE: Is it really such a good idea to store received content
		% in the PD? If not, where does this go...
		put(?CONTENT, Body),
		Body;
	Value -> Value
	end.

%%
set_realm(Realm) when is_atom(Realm) ->
	%% IMPL: enforce write once to PD -- NOTE: for persistent connections too!
	undefined = erlang:put(?REALM, Realm).
%%
realm() ->
	erlang:get(?REALM).

%%
should_close() ->
	Version =/= {1,1}
	orelse header(connection) =:= <<"close">>
	%% unread data left on the socket, can't safely continue
	orelse (erlang:get(?CONTENT) =:= undefined 
		andalso header(content_length) =/= undefined
		andalso header(content_length) > 0).


%% change these signatures?
get_range() ->
    case header(range) of
        undefined ->
            undefined;
        RawRange ->
            parse_range_request(RawRange)
    end.
	
path() ->
%	io:format("GET PATH FROM URL ~p~n", [Url]),
	case erlang:get(?PATH) of
	undefined ->
		%% NOTE: we'd like not to retrun a 'plain' string here. However, there's 
		%% a few dependencies we need to address before removing {return, list} 
		%% from this call.
		case ewok_text:split(Url, <<"\\?">>, 2) of
		[Path, _Query] -> Path;
		[Path] -> Path
		end,
		put(?PATH, Path),
		Path;
	Path -> Path
	end.
%
pathlist() ->	
	ewok_text:split(path(), <<$/>>).

header(K) when is_atom(K) ->
	header(ewok_http:header(K));
% If you ever put back the return value 'undefined' for the header value 
%% then change this too...
%	case ewok_http:header(K) of
%	undefined -> undefined;
%	Value when is_binary(Value) -> header(Value)
%	end;
header(K) when is_list(K) ->
	case proplists:get_value(list_to_binary(K), Headers) of
	undefined -> undefined;
	Value -> binary_to_list(Value)
	end;
header(K) when is_binary(K) ->
	proplists:get_value(K, Headers).

%%
cookie(Key) when is_list(Key) ->
	case proplists:get_value(list_to_binary(Key), cookie()) of 
	undefined -> undefined;
	Value -> binary_to_list(Value)
	end;
cookie(Key) when is_binary(Key) ->
	proplists:get_value(Key, cookie()).

% LATER: also implement "Cookie2"
cookie() -> % [{string(), string{}}] | undefined
	case erlang:get(?COOKIE) of
	undefined ->
		Cookie = 
			case header(<<"Cookie">>) of 
			undefined -> [];
			Value -> parse_cookie(Value)
			end,
		put(?COOKIE, Cookie),
		Cookie;
	Cookie -> Cookie
	end.
%%
parse_cookie(Value) ->
	Props = [X || X <- ewok_text:split(Value, <<";">>)],
	Pairs = [list_to_tuple(ewok_text:split(X, <<"=">>, 2)) || X <- Props],
	[{ewok_text:trim(K), ewok_text:trim(V)} || {K, V} <- Pairs].

%% Query String from GET ? and POST of WWW Forms
parameter(Key) when is_list(Key) ->
	case proplists:get_value(list_to_binary(Key), get_query()) of 
	undefined -> undefined;
	Value -> binary_to_list(Value)
	end;
%
parameter(Key) when is_binary(Key) ->
	proplists:get_value(Key, get_query()).
%
parameters() ->
	get_query().
%%
get_query() ->
	case erlang:get(?QUERY) of
	undefined -> 
		Query = parse_query(Method),
		undefined = put(?QUERY, Query),
		Query;
	Value -> Value
	end.
%%
parse_query('GET') ->
	case ewok_text:split(Url, <<"\\?">>, 2) of
	[_, QS] -> parse_query(QS);
	_ -> []
	end;
parse_query('POST') ->
	case header(content_type) of
	<<"application/x-www-form-urlencoded">> -> 
		case content() of
		QS when is_binary(QS) -> parse_query(QS);
		_ -> []
		end;
	<<"multipart/form-data", _/binary>> ->
		%% TODO: Take boundary from this header
		parse_multipart(content());
	_ -> []
	end;
parse_query(QS) ->
	Props = [X || X <- ewok_text:split(QS, <<"&">>)],
	Pairs = [list_to_tuple(ewok_text:split(X, <<"=">>)) || X <- Props],
	[{ewok_http:url_decode(X), ewok_http:url_decode(Y)} || {X, Y} <- Pairs].	

%%
recv(Length) -> 
	recv(Length, ?IDLE_TIMEOUT).
%% Timeout in msec.
recv(Length, IdleTimeout) ->
    case ewok_socket:recv(Socket, Length, IdleTimeout) of
	{ok, Data} ->
%		put(?BODY_RECEIVE, true),
		Data;
	_ ->
		exit(normal)
    end.

read_chunked_body(Max, Acc) ->
    case read_chunk_length() of
	0 ->
		read_chunk(0),
		iolist_to_binary(lists:reverse(Acc));
	Length when Length > Max ->
		exit({body_too_large, chunked});
	Length ->
		read_chunked_body(Max - Length, [read_chunk(Length) | Acc])
    end.

%% @spec read_chunk_length() -> integer()
%% @doc Read the length of the next HTTP chunk.
read_chunk_length() ->
    inet:setopts(Socket, [{packet, line}]),
    case ewok_socket:recv(Socket, 0, ?IDLE_TIMEOUT) of
        {ok, Header} ->
            inet:setopts(Socket, [{packet, raw}]),
            Splitter = 
				fun (C) ->
					C =/= $\r andalso C =/= $\n andalso C =/= $ 
				end,
            {Hex, _Rest} = lists:splitwith(Splitter, binary_to_list(Header)),
            mochihex:to_int(Hex);
        _ ->
            exit(normal)
    end.

%% @spec read_chunk(integer()) -> Chunk::binary() | [Footer::binary()]
%% @doc Read in a HTTP chunk of the given length. If Length is 0, then read the
%%      HTTP footers (as a list of binaries, since they're nominal).
read_chunk(0) ->
    inet:setopts(Socket, [{packet, line}]),
    F = fun (F1, Acc) ->
			case ewok_socket:recv(Socket, 0, ?IDLE_TIMEOUT) of
			{ok, <<"\r\n">>} -> Acc;
			{ok, Footer} -> F1(F1, [Footer | Acc]);
			_ -> exit(normal)
			end
        end,
    Footers = F(F, []),
    inet:setopts(Socket, [{packet, raw}]),
    Footers;
%%
read_chunk(Length) ->
    case ewok_socket:recv(Socket, 2 + Length, ?IDLE_TIMEOUT) of
	{ok, <<Chunk:Length/binary, "\r\n">>} -> Chunk;
	_ -> exit(normal)
    end.

%% Internal API

%% MULTIPART FORM-DATA
%% Primarily file upload...
parse_multipart(Content) ->
	[Boundary, Rest] = ewok_text:split(Content, <<"\r\n">>, 2),
	Parts = [ewok_text:trim(X) || X <- ewok_text:split(Rest, Boundary)],
	parse_multipart(Parts, []).
%	
parse_multipart([], Acc) ->
	lists:reverse(Acc);
parse_multipart([<<"--">>|_], Acc) ->
	lists:reverse(Acc);
parse_multipart([H|T], Acc) ->
	[Prefix, Value] = ewok_text:split(H, <<"\r\n\r\n">>, 2),
	Attrs = [ewok_text:trim(X) || X <- re:split(Prefix, <<"\r\n">>)],
	Properties = parse_multipart_headers(Attrs, []),
	{value, {<<"name">>, Name}, Properties1} = lists:keytake(<<"name">>, 1, Properties),
	parse_multipart(T, [{Name, {Properties1, Value}}|Acc]).
%	
parse_multipart_headers([], Acc) ->
	lists:reverse(Acc);
parse_multipart_headers([H|T], Acc) ->
	case ewok_text:split(H, <<$:>>) of
	[<<"Content-Disposition">>, Value] -> 
		Params = parse_multipart_disposition(ewok_text:trim(Value)),
		parse_multipart_headers(T, lists:append(Params, Acc));
	[<<"Content-Type">>, Value] -> 
		parse_multipart_headers(T, [{<<"mimetype">>, ewok_text:trim(Value)}|Acc]);
	_ ->
		{error, H}
	end.
%
parse_multipart_disposition(Value) ->
	[<<"form-data">> | Parts] = ewok_text:split(Value, <<$;>>),
	Pairs = [list_to_tuple(ewok_text:split(X, <<"=">>)) || X <- Parts],
	Pairs1 = [{ewok_text:trim(X), ewok_text:trim(Y)} || {X, Y} <- Pairs],
	[{ewok_http:url_decode(X), ewok_http:url_decode(ewok_text:unquote(Y))} || {X, Y} <- Pairs1].

%
strip_quotes(X) ->
	re:replace(X, <<$">>, <<>>, [global, {return, binary}]).

%%
parse_range_request(RawRange) when is_list(RawRange) ->
    try
        "bytes=" ++ RangeString = RawRange,
        Ranges = string:tokens(RangeString, ","),
		F = fun ("-" ++ V)  ->
				{none, list_to_integer(V)};
				(R) ->
				case string:tokens(R, "-") of
				[S1, S2] -> {list_to_integer(S1), list_to_integer(S2)};
				[S] -> {list_to_integer(S), none}
				end
			end,
        lists:map(F, Ranges)
    catch
        _:_ -> fail
    end.

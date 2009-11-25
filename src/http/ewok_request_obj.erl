%% 
-module(ewok_request_obj, [Transport, Socket, Timeout, Method, Url, Version, Headers, MaxHeaders]).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("../include/ewok.hrl").
-include_lib("kernel/include/file.hrl").

-export([get_range/0, get_body_length/0, socket/0, timeout/0, max_headers/0,
	remote_ip/0, version/0, url/0, headers/0, header/1, content/0, 
	path/0, method/0, realm/0, set_realm/1]).
-export([recv/1, recv/2, recv_body/0, recv_body/1, should_close/0]).
-export([cookie/0, cookie/1, parameter/1, parameters/0]).
-export([reset/0]).

%%
-define(REALM, www_request_realm).
-define(PATH, www_request_path).
-define(REMOTE_IP, www_request_ip).
-define(COOKIE, www_request_cookie).
-define(QUERY, www_request_query).
-define(BODY_RECEIVE, www_request_body).
-define(BODY_LENGTH, www_request_length).
-define(BODY_DATA, www_request_body).
-define(CACHE, [?REALM, ?PATH, ?REMOTE_IP, ?COOKIE, ?QUERY, ?BODY_LENGTH, ?BODY_DATA]).

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
	
socket() -> {Transport, Socket}.
timeout() -> Timeout.
max_headers() -> MaxHeaders.
version() -> Version.
url() -> Url.
method() -> Method.
headers() -> Headers.
remote_ip() ->
	case erlang:get(?REMOTE_IP) of
	undefined ->
		IP = ewok_http:get_remote_ip(Transport, Socket, header(<<"X-Forwarded-For">>)),
		erlang:put(?REMOTE_IP, IP),
		IP;
	IP -> IP
	end.

content() -> 
	case erlang:get(?BODY_DATA) of
	undefined -> 
		Data = recv_body(),
		erlang:put(?BODY_DATA, Data);
	Value -> Value
	end.

%%
set_realm(Realm) when is_atom(Realm) ->
	%% IMPL: enforces write once to PD
	undefined = put(?REALM, Realm).
%%
realm() ->
	get(?REALM).

%%
should_close() ->
	Version =/= {1,1} 
	orelse header(connection) =:= <<"close">>
	%% unread data left on the socket, can't safely continue
	orelse (erlang:get(?BODY_RECEIVE) =:= undefined 
		andalso header(content_length) =/= undefined
		andalso header(content_length) > 0).


%% change these signatures?
get_body_length() ->
    erlang:get(?BODY_LENGTH).
	
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
		case re:split(Url, "\\?", [{parts, 2}, {return, list}]) of
		[Path, _Query] -> Path;
		[Path] -> Path
		end,
		put(?PATH, Path),
		Path;
	Path -> Path
	end.
%

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
parse_cookie(Value) ->
	Props = [X || X <- re:split(Value, ";")],
	Pairs = [list_to_tuple(re:split(X, "=", [])) || X <- Props],
	[{ewok_util:trim(K), ewok_util:trim(V)} || {K, V} <- Pairs].

%% Query String from GET ? and POST of WWW Forms
parameter(Key) when is_list(Key) ->
	case proplists:get_value(list_to_binary(Key), get_query()) of 
	undefined -> undefined;
	Value -> binary_to_list(Value)
	end;
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
		put(?QUERY, Query),
		Query;
	Value -> Value
	end.
%%
parse_query('GET') ->
	case re:split(Url, "\\?", [{parts, 2}]) of
	[_, QS] -> parse_query(QS);
	_ -> []
	end;
parse_query('POST') ->
	case header(<<"Content-Type">>) of
	<<"application/x-www-form-urlencoded">> -> 
		case get(?BODY_DATA) of
		undefined ->
			case recv_body() of
			QS when is_binary(QS) -> parse_query(QS);
			_ -> []
			end;
		Value -> parse_query(Value)
		end;
	_ -> []
	end;
parse_query(QS) ->
	Props = [X || X <- re:split(QS, "&")],
	Pairs = [list_to_tuple(re:split(X, "=")) || X <- Props],
	[{ewok_http:url_decode(X), ewok_http:url_decode(Y)} || {X, Y} <- Pairs].	

%%
get_content_length() ->
    case header(<<"Transfer-Encoding">>) of
	undefined ->
		case header(<<"Content-Length">>) of
		undefined -> undefined;
		Length -> list_to_integer(binary_to_list(Length))
		end;
	<<"chunked">> -> chunked;
	Unknown -> {unknown_transfer_encoding, Unknown}
    end.

%%% Deal with request content

%%
recv(Length) -> 
	recv(Length, ?IDLE_TIMEOUT).
%% Timeout in msec.
recv(Length, IdleTimeout) ->
    case Transport:recv(Socket, Length, IdleTimeout) of
	{ok, Data} ->
		put(?BODY_RECEIVE, true),
		Data;
	_ ->
		exit(normal)
    end.

%%
recv_body() ->
    recv_body(?MAX_RECV_BODY).
%
recv_body(MaxBody) ->
    case header(<<"Expect">>) of
	<<"100-continue">> -> ewok_response:continue(THIS);
	_ -> ok %% expectation failed...
    end,
    Body = 
		case get_content_length() of
		undefined -> 
			undefined;
		{unknown_transfer_encoding, Unknown} ->
			exit({unknown_transfer_encoding, Unknown});
		chunked ->
			read_chunked_body(MaxBody, []);
		0 ->
			<<>>;
		Length when is_integer(Length), Length =< MaxBody ->
			recv(Length);
		Length ->
			exit({body_too_large, Length})
		end,
	% NOTE: Is it really such a good idea to store received content
	% in the PD? If not, where does this go...
    put(?BODY_DATA, Body),
    Body.

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
    case Transport:recv(Socket, 0, ?IDLE_TIMEOUT) of
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
			case Transport:recv(Socket, 0, ?IDLE_TIMEOUT) of
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
    case Transport:recv(Socket, 2 + Length, ?IDLE_TIMEOUT) of
	{ok, <<Chunk:Length/binary, "\r\n">>} -> Chunk;
	_ -> exit(normal)
    end.

%% Internal API
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

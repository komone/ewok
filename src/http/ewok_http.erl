%%
-module(ewok_http).

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

-define(SP, <<" ">>).
-define(CRLF, <<"\r\n">>).
-define(EOH, <<"\r\n\r\n">>).

-export([absolute_uri/1, absolute_uri/2, get_remote_ip/2, browser_detect/1]).
-export([status/1, status_type/1, status_code/1, status_message/1, header/1, convert_header/1]).
-export([mimetype/1, date/0, date/1]). 

-export([absolute_uri_couch/1]).

-compile(export_all).

%-record(http_message, {status, headers=[], body=[]}).

absolute_uri(Path) -> 
	absolute_uri(<<"http">>, Path).
%% 
absolute_uri(Protocol, Path) when is_binary(Protocol) -> 
	{ok, Host} = inet:gethostname(),
	%% TODO: perhaps ensure FQDN by adding inet:gethostbyname(Host), ?
	Port =
		case ewok:config({ewok, http, port}) of
		undefined -> <<>>;
		80 -> <<>>;
		Value -> [<<":">>, integer_to_list(Value)]
		end,
	list_to_binary([Protocol, <<"://">>, string:to_lower(Host), Port, Path]).
	
%% Couchdb version...
absolute_uri_couch(Request) -> 
    Host = 
		case Request:header(<<"Host">>) of 
		undefined -> 
			{_, Socket} = Request:socket(),
			{ok, {Address, Port}} = inet:sockname(Socket), 
            inet_parse:ntoa(Address) ++ ":" ++ integer_to_list(Port); 
		Value -> Value 
		end, 
	"http://" ++ Host ++ Request:path().

%%
get_remote_ip(Socket, ProxyHeader) ->
	case ewok_socket:peername(Socket) of
	{ok, {Addr = {10, _, _, _}, _Port}} ->
		case ProxyHeader of
		undefined ->
			list_to_binary(inet_parse:ntoa(Addr));
		Hosts ->
			ewok_text:trim(lists:last(ewok_text:split(Hosts, <<",">>)))
		end;
	{ok, {{127, 0, 0, 1}, _Port}} ->
		case ProxyHeader of
		undefined ->
			<<"127.0.0.1">>;
		Hosts ->
			ewok_text:trim(lists:last(ewok_text:split(Hosts, <<",">>)))
		end;
	{ok, {Addr, _Port}} ->
		list_to_binary(inet_parse:ntoa(Addr))
	end.

%%
browser_detect(UserAgent) when is_binary(UserAgent) ->
	Detect = 
		fun (Match, Acc) ->
			case re:run(UserAgent, Match) of
			{match, [{Start, Length}|_]} ->
				<<_:Start/binary, Value:Length/binary, _/binary>> = UserAgent, 
				Value;
			_ -> Acc
			end
		end, 
	Browser = lists:foldr(Detect, UserAgent, browser_signatures()),
	%% maybe add OS later...
	% OS = lists:foldr(Detector, UserAgent, platform_signatures()),
	Browser.
%%
browser_signatures() -> [
	"Chrome/[0-9\\.]+", %% must be before Safari
	"FireFox/[0-9\\.]+", 
	"Lynx/[0-9\\.]+", 
	"MSIE [0-9\\.]+", %% IE has to be different of course
	"Opera/[0-9\\.]+", 
	"Safari/[0-9\\.]+"
].

%% placeholder...
% platform_signatures() -> [].

%% API
mimetype(Type) when is_binary(Type) ->
	case ewok_db:lookup(ewok_mimetype, Type) of
	undefined -> <<"application/octet-stream">>;
	#ewok_mimetype{media=Value} -> Value;
	Error -> Error
	end;
mimetype(Type) when is_atom(Type) ->
	mimetype([$.|atom_to_list(Type)]);
mimetype(FileExt = [$.|_]) ->
	mimetype(list_to_binary(FileExt)).
%
date() ->
	%calendar:day_of_the_week({Y, Mo, D}),
	%[integer_to_list(Y), "", element(Mo, ?MONTHS)],
	%.....
	list_to_binary(httpd_util:rfc1123_date()).
%
date(LocalDateTime) ->
	list_to_binary(httpd_util:rfc1123_date(LocalDateTime)).

%% ewok_codec
%%
encode(#http_response{status = Status, headers = Headers, content = Content, close = Close}) when is_binary(Content) ->
	Length = size(Content),
	%?TTY({Status, Headers, Length}),
	Headers0 = 
		case proplists:get_value(content_length, Headers) of
		Length ->
			Headers;
		undefined ->
			[{content_length, Length} | Headers]
		end,
	Headers1 = 
		case Close of
		false ->
			Headers0;
		true ->
			[{connection, close} | Headers0]
		end,
	Response = [
		<<"HTTP/1.1 ">>,
		ewok_text:encode(status_code(Status)), ?SP,
		status_message(Status), ?CRLF,
		encode_headers([
			{server, ?SERVER_ID}, 
			{date, ewok_http:date()}
			| Headers1], []),
		?CRLF,
		Content
	],
	{ok, list_to_binary(Response)}.

encode_headers([{K, V}|T], Acc) ->
	Header = [header(K), <<":">>, ?SP, ewok_text:encode(V), ?CRLF],
	encode_headers(T, [list_to_binary(Header) | Acc]);
encode_headers([], Acc) ->
	list_to_binary(lists:reverse(Acc)).

%%
decode(Bin) ->
	[Head | Body] = ewok_text:split(Bin, ?EOH, 2),
%	?TTY(Head),
	[Command | Lines] = ewok_text:split(Head, ?CRLF),
	[Method, FullPath, <<"HTTP/", Version/binary>>] = ewok_text:split(Command, ?SP),
	[Path | Query] = ewok_text:split(FullPath, <<"\\?">>, 2),
	Headers = decode_headers(Lines, []),
	Params = decode_params(ewok_text:split(Query, <<"&">>), []),
	Request = #http_request{
		method = method(Method),
		version = ewok_util:decode_version(Version),
		path = Path,
		headers = Headers,
		params = Params,
		content = Body
	},
	{ok, Request}.
%%
decode_headers([H|T], Acc) ->
	[Name, Value] = ewok_text:split(H, <<":">>, 2),
	decode_headers(T, [{convert_header(ewok_text:trim(Name)), ewok_text:trim(Value)} | Acc]);
decode_headers([], Acc) ->
	lists:reverse(Acc).
%%
decode_params([H|T], Acc) ->
	[K | V] = ewok_text:split(H, <<"=">>, 2),
	decode_params(T, [{K, list_to_binary(V)} | Acc]);
decode_params([], Acc) ->
	lists:reverse(Acc).

method(<<"HEAD">>) -> 'HEAD';
method(<<"GET">>) -> 'GET';
method(<<"POST">>) -> 'POST';
method(<<"PUT">>) -> 'PUT';
method(<<"TRACE">>) -> 'TRACE';
method(<<"OPTIONS">>) -> 'OPTIONS'.

%%
status(X) when is_integer(X) ->
	status_type(X);
status(X) when is_atom(X) ->
	status_code(X).
	
% HTTP Status Codes
status_type(100) -> continue;
status_type(101) -> switching_protocols;
status_type(200) -> ok;
status_type(201) -> created;
status_type(202) -> accepted;
status_type(203) -> non_authoritative_information;
status_type(204) -> no_content;
status_type(205) -> reset_content;
status_type(206) -> partial_content;
status_type(207) -> multi_status;
status_type(300) -> multiple_choices;
status_type(301) -> moved_permanently;
status_type(302) -> found;
status_type(303) -> see_other;
status_type(304) -> not_modified;
status_type(305) -> use_proxy;
status_type(306) -> unused;
status_type(307) -> temporary_redirect;
status_type(400) -> bad_request;
status_type(401) -> unauthorized;
status_type(402) -> payment_required;
status_type(403) -> forbidden;
status_type(404) -> not_found;
status_type(405) -> method_not_allowed;
status_type(406) -> not_acceptable;
status_type(407) -> proxy_authentication_required;
status_type(408) -> request_timeout;
status_type(409) -> conflict;
status_type(410) -> gone;
status_type(411) -> length_required;
status_type(412) -> precondition_failed;
status_type(413) -> request_entity_too_large;
status_type(414) -> request_uri_too_long;
status_type(415) -> unsupported_media_type;
status_type(416) -> request_range_not_satisfiable;
status_type(417) -> expectation_failed;
status_type(500) -> internal_server_error;
status_type(501) -> not_implemented;
status_type(502) -> bad_gateway;
status_type(503) -> service_unavailable;
status_type(504) -> gateway_timeout;
status_type(505) -> http_version_not_supported.

status_code(Int) when is_integer(Int)      -> Int;
status_code(continue)                      -> 100;
status_code(switching_protocols)           -> 101;
status_code(ok)                            -> 200;
status_code(created)                       -> 201;
status_code(accepted)                      -> 202;
status_code(non_authoritative_information) -> 203;
status_code(no_content)                    -> 204;
status_code(reset_content)                 -> 205;
status_code(partial_content)               -> 206;
status_code(multi_status)                  -> 207;
status_code(multiple_choices)              -> 300;
status_code(moved_permanently)             -> 301;
status_code(found)                         -> 302;
status_code(see_other)                     -> 303;
status_code(not_modified)                  -> 304;
status_code(use_proxy)                     -> 305;
status_code(unused)                        -> 306;
status_code(temporary_redirect)            -> 307;
status_code(bad_request)                   -> 400;
status_code(unauthorized)                  -> 401;
status_code(payment_required)              -> 402;
status_code(forbidden)                     -> 403;
status_code(not_found)                     -> 404;
status_code(method_not_allowed)            -> 405;
status_code(not_acceptable)                -> 406;
status_code(proxy_authentication_required) -> 407;
status_code(request_timeout)               -> 408;
status_code(conflict)                      -> 409;
status_code(gone)                          -> 410;
status_code(length_required)               -> 411;
status_code(precondition_failed)           -> 412;
status_code(request_entity_too_large)      -> 413;
status_code(request_uri_too_long)          -> 414;
status_code(unsupported_media_type)        -> 415;
status_code(request_range_not_satisfiable) -> 416;
status_code(expectation_failed)            -> 417;
status_code(internal_server_error)         -> 500;
status_code(not_implemented)               -> 501;
status_code(bad_gateway)                   -> 502;
status_code(service_unavailable)           -> 503;
status_code(gateway_timeout)               -> 504;
status_code(http_version_not_supported)    -> 505.

% Standard HTTP Status Messages
status_message(X) when is_atom(X) ->
	status_message(status_code(X));
status_message(100) -> <<"Continue">>;
status_message(101) -> <<"Switching Protocols">>;
status_message(200) -> <<"OK">>;
status_message(201) -> <<"Created">>;
status_message(202) -> <<"Accepted">>;
status_message(203) -> <<"Non-Authoritative Information">>;
status_message(204) -> <<"No Content">>;
status_message(205) -> <<"Reset Content">>;
status_message(206) -> <<"Partial Content">>;
status_message(207) -> <<"Multi Status">>;
status_message(300) -> <<"Multiple Choices">>;
status_message(301) -> <<"Moved Permanently">>;
status_message(302) -> <<"Found">>;
status_message(303) -> <<"See Other">>;
status_message(304) -> <<"Not Modified">>;
status_message(305) -> <<"Use Proxy">>;
status_message(306) -> <<"(Unused)">>;
status_message(307) -> <<"Temporary Redirect">>;
status_message(400) -> <<"Bad Request">>;
status_message(401) -> <<"Unauthorized">>;
status_message(402) -> <<"Payment Required">>;
status_message(403) -> <<"Forbidden">>;
status_message(404) -> <<"Not Found">>;
status_message(405) -> <<"Method Not Allowed">>;
status_message(406) -> <<"Not Acceptable">>;
status_message(407) -> <<"Proxy Authentication Required">>;
status_message(408) -> <<"Request Timeout">>;
status_message(409) -> <<"Conflict">>;
status_message(410) -> <<"Gone">>;
status_message(411) -> <<"Length Required">>;
status_message(412) -> <<"Precondition Failed">>;
status_message(413) -> <<"Request Entity Too Large">>;
status_message(414) -> <<"Request-URI Too Long">>;
status_message(415) -> <<"Unsupported Media Type">>;
status_message(416) -> <<"Requested Range Not Satisfiable">>;
status_message(417) -> <<"Expectation Failed">>;
status_message(500) -> <<"Internal Server Error">>;
status_message(501) -> <<"Not Implemented">>;
status_message(502) -> <<"Bad Gateway">>;
status_message(503) -> <<"Service Unavailable">>;
status_message(504) -> <<"Gateway Timeout">>;
status_message(505) -> <<"HTTP Version Not Supported">>.

% HTTP Header Names
header(accept) ->              <<"Accept">>;
header(accept_charset) ->      <<"Accept-Charset">>;
header(accept_encoding) ->     <<"Accept-Encoding">>;
header(accept_language) ->     <<"Accept-Language">>;
header(accept_ranges) ->       <<"Accept-Ranges">>;
header(age) ->                 <<"Age">>;
header(allow) ->               <<"Allow">>;
header(authorization) ->       <<"Authorization">>;
header(cache_control) ->       <<"Cache-Control">>;
header(connection) ->          <<"Connection">>;
header(content_encoding) ->    <<"Content-Encoding">>;
header(content_disposition) -> <<"Content-Disposition">>;
header(content_language) ->    <<"Content-Language">>;
header(content_length) ->      <<"Content-Length">>;
header(content_location) ->    <<"Content-Location">>;
header(content_md5) ->         <<"Content-MD5">>;
header(content_range) ->       <<"Content-Range">>;
header(content_type) ->        <<"Content-Type">>;
header(date) ->                <<"Date">>;
header(etag) ->                <<"ETag">>;
header(expect) ->              <<"Expect">>;
header(expires) ->             <<"Expires">>;
header(from) ->                <<"From">>;
header(host) ->                <<"Host">>;
header(if_match) ->            <<"If-Match">>;
header(if_modified_since) ->   <<"If-Modified-Since">>;
header(if_none_match) ->       <<"If-None-Match">>;
header(if_range) ->            <<"If-Range">>;
header(if_unmodified_since) -> <<"If-Unmodified-Since">>;
header(last_modified) ->       <<"Last-Modified">>;
header(location) ->            <<"Location">>;
header(max_forwards) ->        <<"Max-Forwards">>;
header(pragma) ->              <<"Pragma">>;
header(proxy_authenticate) ->  <<"Proxy-Authenticate">>;
header(proxy_authorization) -> <<"Proxy-Authorization">>;
header(range) ->               <<"Range">>;
header(referer) ->             <<"Referer">>;
header(retry_after) ->         <<"Retry-After">>;
header(server) ->              <<"Server">>;
header(te) ->                  <<"TE">>;
header(trailer) ->             <<"Trailer">>;
header(transfer_encoding) ->   <<"Transfer-Encoding">>;
header(upgrade) ->             <<"Upgrade">>;
header(user_agent) ->          <<"User-Agent">>;
header(vary) ->                <<"Vary">>;
header(via) ->                 <<"Via">>;
header(warning) ->             <<"Warning">>;
header(www_authenticate) ->    <<"WWW-Authenticate">>;
%% Extensions to HTTP/1.1
header(cookie) ->              <<"Cookie">>;
header(set_cookie) ->          <<"Set-Cookie">>;
header(origin) ->              <<"Origin">>;
%% WebSockets Extension
header(WebSocket = <<"WebSocket-", _R/binary>>) -> WebSocket;
%% 'X' Headers
header(X = <<"X-", _R/binary>>) -> X.
%% Don't return undefined if not found... ewok_request gets unhappy.

% HTTP Header Keys
convert_header(<<"Accept">>) ->              accept;                       
convert_header(<<"Accept-Charset">>) ->      accept_charset;               
convert_header(<<"Accept-Encoding">>) ->     accept_encoding;              
convert_header(<<"Accept-Language">>) ->     accept_language;              
convert_header(<<"Accept-Ranges">>) ->       accept_ranges;                
convert_header(<<"Age">>) ->                 age;                          
convert_header(<<"Allow">>) ->               allow;                        
convert_header(<<"Authorization">>) ->       authorization;                
convert_header(<<"Cache-Control">>) ->       cache_control;                
convert_header(<<"Connection">>) ->          connection;                   
convert_header(<<"Content-Encoding">>) ->    content_encoding;             
convert_header(<<"Content-Disposition">>) -> content_disposition;          
convert_header(<<"Content-Language">>) ->    content_language;             
convert_header(<<"Content-Length">>) ->      content_length;               
convert_header(<<"Content-Location">>) ->    content_location;             
convert_header(<<"Content-MD5">>) ->         content_md5;                  
convert_header(<<"Content-Range">>) ->       content_range;                
convert_header(<<"Content-Type">>) ->        content_type;                 
convert_header(<<"Date">>) ->                date;                         
convert_header(<<"ETag">>) ->                etag;                         
convert_header(<<"Expect">>) ->              expect;                       
convert_header(<<"Expires">>) ->             expires;                      
convert_header(<<"From">>) ->                from;                         
convert_header(<<"Host">>) ->                host;                         
convert_header(<<"If-Match">>) ->            if_match;                     
convert_header(<<"If-Modified-Since">>) ->   if_modified_since;            
convert_header(<<"If-None-Match">>) ->       if_none_match;                
convert_header(<<"If-Range">>) ->            if_range;                     
convert_header(<<"If-Unmodified-Since">>) -> if_unmodified_since;          
convert_header(<<"Last-Modified">>) ->       last_modified;                
convert_header(<<"Location">>) ->            location;                     
convert_header(<<"Max-Forwards">>) ->        max_forwards;                 
convert_header(<<"Pragma">>) ->              pragma;                       
convert_header(<<"Proxy-Authenticate">>) ->  proxy_authenticate;           
convert_header(<<"Proxy-Authorization">>) -> proxy_authorization;          
convert_header(<<"Range">>) ->               range;                        
convert_header(<<"Referer">>) ->             referer;                      
convert_header(<<"Retry-After">>) ->         retry_after;                  
convert_header(<<"Server">>) ->              server;                       
convert_header(<<"TE">>) ->                  te;                           
convert_header(<<"Trailer">>) ->             trailer;                      
convert_header(<<"Transfer-Encoding">>) ->   transfer_encoding;            
convert_header(<<"Upgrade">>) ->             upgrade;                      
convert_header(<<"User-Agent">>) ->          user_agent;                   
convert_header(<<"Vary">>) ->                vary;                         
convert_header(<<"Via">>) ->                 via;                          
convert_header(<<"Warning">>) ->             warning;                      
convert_header(<<"WWW-Authenticate">>) ->    www_authenticate;             
%% Extensions to HTTP/1.1      
convert_header(<<"Cookie">>) ->              cookie;              
convert_header(<<"Set-Cookie">>) ->          set_cookie;          
convert_header(<<"Origin">>) ->              origin;              
%% WebSockets Extension                
convert_header(WebSocket = <<"WebSocket-", _/binary>>) -> WebSocket;
%%
convert_header(Soap = <<"SOAP", _/binary>>) -> Soap;
%% 'X' convert_headers  
convert_header(X = <<"X-", _R/binary>>) -> X.     
%% Don't return undefined if not found... ewok_request gets unhappy.

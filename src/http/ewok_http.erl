%%
-module(ewok_http).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl").
-include("ewok_system.hrl").

-export([absolute_uri/1, absolute_uri/2, get_remote_ip/2, browser_detect/1]).
-export([url_encode/1, url_decode/1]).
-export([status/1, status_type/1, status_code/1, status_message/1, header/1]).
-export([mimetype/1, date/0, date/1]). 

-export([absolute_uri_couch/1]).


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

%

%% placeholder...
% platform_signatures() -> [].

%%
url_decode(Bin) when is_binary(Bin) ->
	list_to_binary(url_decode(binary_to_list(Bin), []));
url_decode(List) when is_list(List) ->
	url_decode(List, []).	
%
url_decode([$%, Hi, Lo|T], Acc) ->
	try 
		Char = erlang:list_to_integer([Hi, Lo], 16),
		url_decode(T, [Char|Acc])
	catch
		error:badarg -> {error, invalid_encoding}
	end;
url_decode([$+|T], Acc) ->
	url_decode(T, [$ |Acc]);
url_decode([H|T], Acc) ->
	url_decode(T, [H|Acc]);
url_decode([], Acc) ->
	lists:reverse(Acc).
	
%%
url_encode(Bin) when is_binary(Bin) ->
	list_to_binary(url_encode(binary_to_list(Bin), []));
url_encode(List) when is_list(List) ->
	url_encode(List, []).	
%
url_encode([H|T], Acc) ->
	url_encode(T, [url_encode_char(H)|Acc]);
url_encode([], Acc) ->
	lists:flatten(lists:reverse(Acc)).

url_encode_char(C) when C >= $a, C =< $z -> C;
url_encode_char(C) when C >= $A, C =< $Z -> C;
url_encode_char(C) when C >= $0, C =< $9 -> C;
url_encode_char(C = $~) -> C;
url_encode_char(C = $_) -> C;
url_encode_char(C = $.) -> C;
url_encode_char(C = $-) -> C;
url_encode_char($ ) -> "%20";
url_encode_char($!) -> "%21";
url_encode_char($#) -> "%23";
url_encode_char($$) -> "%24";
url_encode_char($%) -> "%25";
url_encode_char($&) -> "%26";
url_encode_char($') -> "%27";
url_encode_char($() -> "%28";
url_encode_char($)) -> "%29";
url_encode_char($*) -> "%2A";
url_encode_char($+) -> "%2B"; % should this always be encoded?
url_encode_char($,) -> "%2C";
url_encode_char($/) -> "%2F";
url_encode_char($:) -> "%3A";
url_encode_char($;) -> "%3B";
url_encode_char($=) -> "%3D";
url_encode_char($@) -> "%40";
url_encode_char($?) -> "%3F";
url_encode_char($[) -> "%5B";
url_encode_char($]) -> "%5D".
% No other characters are valid

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
status_type(505) -> http_version_not_supported;
status_type(X) when is_integer(X) -> X.

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


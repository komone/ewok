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

-module(ewok_http_cookie).
-include("ewok.hrl").

-export([parse_cookies/1]).
-export([start/0, gen_auth/1, gen_build/2, gen_check/2, read/1, check/4, test/0]).
-export([make/1, message/1]).

%% TEMP
-define(SERVER_KEY, <<"secret">>).
-define(IVEC, <<213,53,164,93,158,212,70,56,134,80,224,220,249,214,82,76>>).

%% Used by ewok_http_inet
parse_cookies(#http_request{headers = Headers}) ->
	Cookie = proplists:get_value(cookie, Headers, []),
	Values = [X || X <- ewok_text:split(Cookie, <<";">>)],
	Pairs = [list_to_tuple(ewok_text:split(X, <<"=">>, 2)) || X <- Values],
	[{ewok_text:trim(K), ewok_text:trim(V)} || {K, V} <- Pairs].

%% The below functions are for future functionality, defining an algorithm for "secure cookies" - requires SSL
% http://www.cse.msu.edu/~alexliu/publications/Cookie/cookie.ppt
% @def http://www.cse.msu.edu/~alexliu/publications/Cookie/cookie.pdf
% username | expiration time | (data)k | HMAC(username | expiration time | data | ssl session key, k)
% where k = HMAC(user name | expiration time, sk)

% Cookie Verification
% Input :Acookie
% Output : TRUE if the cookie is valid; FALSE otherwise
% 1. Compare the cookie’s expiration time and the server’s current time. If the cookie has expired, then return FALSE.
% 2. Compute the encryption key as follows: k=HMAC(user name|expiration time, sk)
% 3. Decrypt the encrypted data using k.
% 4. Compute HMAC(user name|expiration time|data|session key, k), and compare it with the keyed-hash message authentication code
% of the cookie. If they match, then return TRUE; otherwise return FALSE.

start() ->
	application:start(crypto). 


make(#http_session{key = _Key, user = _User, expires = _Expires, data = _Data}) ->
	ok.

gen_build(ServerKey, IVec) ->
	fun(Username, D, SessionKey) ->
		Expiration = integer_to_list(1212559656),
		Key = crypto:md5_mac( [Username, Expiration], ServerKey), %16bytes
		Data = crypto:aes_cbc_128_encrypt(Key, IVec, D),
		Hmac = crypto:sha_mac([Username, Expiration, Data, SessionKey], Key),
		io:format("Build: ~p ~p ~p ~p ~p~n",[Username, Expiration, Data, SessionKey, Key]),
		iolist_to_binary([ Username, $,, Expiration, $,, Data, $,, Hmac ])
	end.

read(Cookie) ->
	{A, B, C} = Cookie,
	{A, B, C}.

 
gen_check(ServerKey, IVec) ->
	fun(Cookie, SessionKey) ->
		[ Username, Expiration, Crypted, Hmac ] = string:tokens(binary_to_list(Cookie), ","),
		Key = crypto:md5_mac([ Username, Expiration ], ServerKey),
		Data = crypto:aes_cbc_128_decrypt(Key, IVec, Crypted),
		MAC = crypto:sha_mac([ Username, Expiration, Crypted, SessionKey], Key),
		io:format("Check: ~p ~p ~p ~p ~p~n",[Username, Expiration, Data, SessionKey, Key]),
		<<Len:16, Message:Len/binary, _/binary>> = Data,
		io:format("Decrypted: ~p '~s'~n'~p'~n'~p'~n", [Len, Message, MAC, list_to_binary(Hmac)]),
		[ Username, Expiration, {Len, Message}, MAC, list_to_binary(Hmac)]
	end.

% Returns the build fun and check fun 
% This is a helper fun to let you build in q simple way bot the build fun and
% the decode fun...
gen_auth(ServerKey) -> 
	IVec = <<"3985928509201031">>, %16bytes Must be Random
	[ gen_build(ServerKey, IVec), gen_check(ServerKey, IVec) ].


check(Cookie, ServerKey, InitVec, SessionKey) ->
	{Username, ExpirationTime, Crypted, CookieMAC} = read(Cookie),
	case check_time(ExpirationTime) of % see later check_time...
	ok ->
		Key = crypto:sha_mac([ Username, ExpirationTime ], ServerKey),
		Data = crypto:aes_cbc_128_decrypt(Key, InitVec, Crypted),
		MAC = crypto:sha_mac([ Username, ExpirationTime, Data, SessionKey], Key),
		compare(MAC, CookieMAC, Data);
	_E -> 
		{error, _E}
	end.

compare(_A, _A, Data) ->
	{ok, Data};
compare(_A, _B, _Data) ->
	{error, nomatch}.

check_time(1212559656) -> % It's up to you to set it 
	true;
check_time(_) ->
	false.

message(Text) ->
	Len = size(Text),
	Pad = 64 - Len - 2,
	<<Len:16, Text/binary, 0:Pad/unit:8>>.

test() ->
	ServerKey = <<"serverkey">>,
	SessionKey = <<"3ID409a0sd09">>,
	[ Enc, Dec ] = gen_auth(ServerKey),
	CCookie = Enc("rolphin", message(<<"stream/128693">>), SessionKey), 
	DCookie = Dec(CCookie, SessionKey),
	io:format("C: ~s~n", [ CCookie ]),
	display(DCookie).

display([Username, Expiration, {Len, Message}, _Mac, _Mac]) ->
	io:format("Message ok: ~s (~s) ~p: '~s'~n", [Username, Expiration, Len, Message]);
display([Username, Expiration, {Len, Message}, _Mac, _OtherMac]) ->
	io:format("Invalid Mac ! ~s (~s) ~p: '~s'~n", [Username, Expiration, Len, Message]).

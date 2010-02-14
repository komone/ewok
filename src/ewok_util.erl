%%%% Copyright 2009 Steve Davis <steve@simulacity.com>
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

-module(ewok_util).

-include("ewok.hrl").

-export([appdir/0, appdir/1, get_env/1, get_env/2, ensure_started/1]).
-export([check_dependencies/1, tcp_ports/0]).
-export([timestamp/0, timestamp/1, unow/0, utime/0, unix_time/0]).
-export([build_number/0, build_time/0]).
-export([trim/1, split/2, split/3, unquote/1, is_upper/1, to_upper/1, is_lower/1, to_lower/1]).
-export([to_binary/1, hex/1, unhex/1, hexint/1]).
-export([ftime/1, ftime/2]).

-define(UNIX_TIME_ZERO, 62167219200).

%%
appdir() ->
	appdir(ewok).
appdir(App) when is_atom(App) ->
	{'module', App} = code:ensure_loaded(App),
	{file, Path} = code:is_loaded(App),
	list_to_binary(filename:dirname(filename:dirname(Path))).

%%
get_env(Key) when is_atom(Key) ->
	get_env(ewok, Key, undefined);
%%
get_env({App, Key}) ->
	get_env(App, Key, undefined).
%%
get_env(Key, Default) when is_atom(Key) ->
	get_env(ewok, Key, Default);
%%
get_env({App, Key}, Default) ->
	get_env(App, Key, Default).
%%
get_env(App, Key, Default) when is_atom(Key) ->
	case application:get_env(App, Key) of
	{ok, Value} -> 
		Value;
	undefined -> 
		Default
	end.
	
%%
ensure_started(Apps) when is_list(Apps) ->
	lists:foldl(fun(X, Acc) -> [ensure_started(X)|Acc] end, [], Apps);
ensure_started(App) when is_atom(App) ->
	case application:start(App) of
	ok -> 
		{ok, App};
	{error, {already_started, App}} -> 
		{ok, App};
	Other -> 
		Other
	end.
	
%%
check_dependencies(Depends) ->
	case [X || X <- Depends, is_pid(whereis(X)) =/= true] of
	[] -> 
		ok;
	NotRunning -> 
		erlang:error(dependency, NotRunning)
	end.

%% 
tcp_ports() ->
	Sockets = [S || S = S1 <- erlang:ports(), 
		erlang:port_info(S1, name) =:= {name, "tcp_inet"}],
	Ports = [inet:port(P) || P <- Sockets],
	[X || {ok, X} <- Ports].

%% TODO: Incorrect
build_number() ->
	DateTime = build(),
	BuildNumber = calendar:datetime_to_gregorian_seconds(DateTime),
	list_to_binary(integer_to_list(BuildNumber)).
%%
build_time() ->
	timestamp(build()).
%%
build() ->
	code:ensure_loaded(ewok),
	{Y, Mo, D, H, M, S} = proplists:get_value(time, ewok:module_info('compile')),
	{{Y, Mo, D}, {H, M, S}}.

%%
trim(S) when ?is_string(S) ->
	string:strip(S);
trim(S) when is_binary(S) -> 
	% @credit Seth Falcon
	re:replace(S, <<"^\\s+|\\s+$">>, <<"">>, [{return, binary}, global]);
trim(S) -> 
	S.

%%
split(Bin, Regex) ->
	split(Bin, Regex, []).
split(Bin, Regex, Parts) when is_integer(Parts) ->
	split(Bin, Regex, [{parts, Parts}]);
split(Bin, Regex, Opts) ->
	[X || X <- re:split(Bin, Regex, Opts), X =/= <<>>].	


%%
unquote(Bin) ->
	re:replace(Bin, <<"^\"|\"$">>, <<"">>, [{return, binary}, global]).

to_binary(X) when is_binary(X) -> X;
to_binary(X) when is_atom(X) -> atom_to_binary(X, utf8);
to_binary(X) when is_integer(X) -> list_to_binary(integer_to_list(X));
to_binary(X) when is_float(X) -> list_to_binary(float_to_list(X));
to_binary(X) when is_tuple(X) -> io_lib:format("~p", [X]);
to_binary(X) when ?is_string(X) -> list_to_binary(X);
to_binary(X) -> io_lib:format("~p", [X]).

%%
is_upper(<<C>>) -> is_upper(C);
is_upper([C]) -> is_upper(C);
is_upper(C) when C >= $A, C =< $Z -> true;
is_upper(C) when C >= 16#C0, C =< 16#D6 -> true;
is_upper(C) when C >= 16#D8, C =< 16#DE -> true;
is_upper(_) -> false.

%% 
to_upper(C) when is_integer(C) -> to_upper(<<C>>);
to_upper(S) when ?is_string(S) -> string:to_upper(S);
to_upper(B) when is_binary(B) -> bin_to_upper(B, <<>>);
to_upper(S) -> S.
% @private
bin_to_upper(<<C, Rest/binary>>, Acc) ->
	U = uppercase(C),
	bin_to_upper(Rest, <<Acc/binary, U>>);
bin_to_upper(<<>>, Acc) ->
	Acc.
%% IMPL: Latin1
uppercase(C) when C >= $a, C =< $z -> C - 32;
uppercase(C) when C >= 16#E0, C =< 16#F6 -> C - 32;
uppercase(C) when C >= 16#F8, C =< 16#FE -> C - 32;
uppercase(C) -> C.

%% 
is_lower(<<C>>) -> is_lower(C);
is_lower([C]) -> is_lower(C);
is_lower(C) when C >= $a, C =< $z -> true;
is_lower(C) when C >= 16#E0, C =< 16#F6 -> true;
is_lower(C) when C >= 16#F8, C =< 16#FE -> true;
is_lower(_) -> false.
	
%%
to_lower(C) when is_integer(C) -> lowercase(C);
to_lower(S) when ?is_string(S) -> string:to_lower(S);
to_lower(B) when is_binary(B) -> bin_to_lower(B, <<>>);
to_lower(V) -> V.
% @private
bin_to_lower(<<C, Rest/binary>>, Acc) ->
	C1 = lowercase(C),
	bin_to_lower(Rest, <<Acc/binary, C1>>);
bin_to_lower(<<>>, Acc) ->
	Acc.
	
lowercase(C) when C >= $A, C =< $Z -> C + 32;
lowercase(C) when C >= 16#C0, C =< 16#D6 -> C + 32;
lowercase(C) when C >= 16#D8, C =< 16#DE -> C + 32;
lowercase(C) -> C.

%%
hex(Bin) when is_binary(Bin) ->
	hex(Bin, <<>>).
hex(<<A:4, B:4, Rest/binary>>, Acc) ->
	U = hexdigit(A),
	L = hexdigit(B),
	hex(Rest, <<Acc/binary, U, L>>);
hex(<<>>, Acc) ->
	Acc.
% @private
hexdigit(D) when D >= 0, D =< 9 -> $0 + D;
hexdigit(D) when D >= 10, D =< 16 -> $a + D - 10.


hexint(Bin) when is_binary(Bin) ->
	hexint(Bin, 0).
hexint(<<A, Rest/binary>>, Acc) ->
	hexint(Rest, unhexdigit(A) + Acc * 16);
hexint(<<>>, Acc) ->
	Acc.

unhex(Bin) when is_binary(Bin) ->
	unhex(Bin, <<>>).
unhex(<<A, B, Rest/binary>>, Acc) ->
	I = unhexdigit(A) * 16 + unhexdigit(B),
	unhex(Rest, <<Acc/binary, I>>);
unhex(<<>>, Acc) ->
	Acc.
% @private
unhexdigit(H) when H >= $0, H =< $9 -> H - $0;
unhexdigit(H) when H >= $a, H =< $f -> H - $a + 10;
unhexdigit(H) when H >= $A, H =< $F -> H - $A + 10.

%% possibly make an ewok_date module
utime() ->
	timestamp().
%
unow() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
%
unix_time() ->
	unow() - ?UNIX_TIME_ZERO.

%%
timestamp() ->
	timestamp(calendar:universal_time()).
timestamp(DateTime = {_, _}) ->
	list_to_binary(to_iso8601(DateTime, "Z")).
%%
to_iso8601({{Year, Month, Day}, {Hour, Min, Sec}}, Zone) ->
	ISO_8601 = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B~s",
	io_lib:format(ISO_8601, [Year, Month, Day, Hour, Min, Sec, Zone]).
	
%
ftime(Fun) ->
	ftime(Fun, 1).
ftime(Fun, Repeats) when is_function(Fun), is_integer(Repeats) ->
	{A1, B1, C1} = erlang:now(),
    exec_function(Fun, Repeats),
	{A2, B2, C2} = erlang:now(),
    Time = ((A2-A1) * 1000000 + (B2-B1)) * 1000 + (C2-C1) / 1000,
	io:format("~p ms~n", [Time]).
%
exec_function(_, 0) -> ok;
exec_function(F, Repeats) ->
	F(),
	exec_function(F, Repeats - 1).

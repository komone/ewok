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

-module(ewok_util).

-include("ewok.hrl").

-export([appdir/0, appdir/1, get_env/1, get_env/2, ensure_started/1]).
-export([check_dependencies/1, check_behaviour/2, tcp_ports/0]).
-export([timestamp/0, timestamp/1, unow/0, utime/0, unix_time/0]).
-export([build_number/0, build_time/0, decode_version/1, encode_version/1]).
-export([ftime/1, ftime/2, key/2, key/3, value/2, value/3]).

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
	
decode_version(Bin) when is_binary(Bin) ->
	List0 = ewok_text:split(Bin, <<"\\.">>),
	List1 = [ewok_text:eval(X) || X <- List0],
	list_to_tuple(List1).
	
encode_version(Term) when is_tuple(Term) ->
	List0 = tuple_to_list(Term),
	List1 = [ewok_text:encode(X) || X <- List0, is_integer(X)],
	ewok_text:interleave(List1, <<$.>>).
	
%%
check_dependencies(Depends) ->
	case [X || X <- Depends, is_pid(whereis(X)) =/= true] of
	[] -> 
		ok;
	NotRunning -> 
		throw({depends, NotRunning})
	end.
%%
check_behaviour(Module, Behaviour) ->
	Info = Module:module_info(attributes),
	List  = [Y || {X, Y} <- Info, X =:= 'behaviour' orelse X =:= 'behavior'],
	case [X || X <- lists:flatten(List), X =:= Behaviour] of
	[Behaviour] -> 
		ok;
	[] ->
		{error, not_found}
	end.
%%
key(V, Properties) ->
	key(V, Properties, undefined).
%%
key(V, Properties, Default) ->
	case lists:keyfind(V, 2, Properties) of
	{K, V} ->
		K;
	false -> 
		Default
	end.
	
%%
value(K, Properties) ->
	value(K, Properties, undefined).
%%
value(K, Properties, Default) ->
	case lists:keyfind(K, 1, Properties) of
	{K, V} ->
		V;
	false ->
		Default
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
	{'module', Module} = code:ensure_loaded(ewok),
	{Y, Mo, D, H, M, S} = proplists:get_value(time, Module:module_info('compile')),
	{{Y, Mo, D}, {H, M, S}}.

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
ftime({M, F, A}) ->
	ftime({M, F, A}, 1).
ftime({M, F, A}, Repeats) when is_integer(Repeats) ->
	{A1, B1, C1} = erlang:now(),
    exec_function({M, F, A}, Repeats),
	{A2, B2, C2} = erlang:now(),
	((A2-A1) * 1000000 + (B2-B1)) * 1000 + (C2-C1) / 1000.
%
exec_function(_, 0) -> ok;
exec_function({M, F, A}, Repeats) ->
	apply(M, F, A),
	exec_function({M, F, A}, Repeats - 1).

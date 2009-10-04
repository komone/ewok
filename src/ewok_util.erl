%%
-module(ewok_util).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-export([appdir/0, appdir/1]).
-export([get_env/1, tcp_ports/0]).
-export([timestamp/0, timestamp/1]).
-export([build_number/0, build_time/0]).
-export([trim/1, unow/0, utime/0]).
-export([ftime/1]).

%%
appdir() ->
	appdir(ewok).
appdir(App) when is_atom(App) ->
	{'module', App} = code:ensure_loaded(App),
	{file, Path} = code:is_loaded(App),
	filename:dirname(filename:dirname(Path)).

	

%% possibly make an ewok_date module
utime() ->
	{{Y, Mo, D}, {H, M, S}} = calendar:universal_time(),
	DateFormat = "~4.10.0B-~2.10.0B-~2.10.0BT~2.10.0B:~2.10.0B:~2.10.0B~s",
	list_to_binary(io_lib:format(DateFormat, [Y, Mo, D, H, M, S, "Z"])).
%
unow() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).


%% 
tcp_ports() ->
	Sockets = [S || S = S1 <- erlang:ports(), 
		erlang:port_info(S1, name) =:= {name, "tcp_inet"}],
	Ports = [inet:port(P) || P <- Sockets],
	[X || {ok, X} <- Ports].

%%
get_env(Key) when is_atom(Key) ->
	{ok, Value} = application:get_env(ewok, Key),
	Value.

%%
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
trim(S) when is_binary(S) -> 
	% @credit Seth Falcon
	re:replace(S, "^\\s+|\\s+$", "", [{return, binary}, global]);
trim(S) when is_list(S) ->
	string:strip(S);
trim(S) -> S.

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

%%
-module(ewok_util).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-compile(export_all).
-export([get_env/1, appdir/0, trim/1]).
-export([timestamp/0, timestamp/1]).

%%
get_env(Key) when is_atom(Key) ->
	{ok, Value} = application:get_env(ewok, Key),
	Value.
	
%%
appdir() ->
	appdir(ewok).
appdir(App) when is_atom(App) ->
	code:ensure_loaded(App),
	{file, Beam} = code:is_loaded(App),
	filename:dirname(filename:dirname(Beam)).
	
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

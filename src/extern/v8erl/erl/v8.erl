-module(v8).
-export([load/0,
         context/0,
         execute/2]).
-export([test/0]).
-define(LIB, "v8erl").
-define(DEFAULT_TIMEOUT, 3000). % ms

load() ->
    case whereis(v8erl) of
        undefined ->
            case erl_ddll:load_driver("lib", ?LIB) of
                ok ->
                    ok;
                {error, already_loaded} ->
                    ok;
                {error, Err} ->
                    {error, erl_ddll:format_error(Err)}
            end;
        _ ->
            ok
    end.

context() ->
    erlang:open_port({spawn, ?LIB}, [binary]).

destroy(Context) when is_port(Context) ->
    erlang:port_close(Context).

execute(Context, Script) when is_port(Context) andalso
			      is_binary(Script) ->
    Key = erlang:phash2(make_ref()),
    erlang:port_command(Context, term_to_binary({Key, Script})),
    Key.

script_result(Context, Script) ->
    io:format("[v8] ~s~n", [Script]),
    Key = execute(Context, Script),
    receive
	{Key, Response} ->
	    Response
    after
	3000 ->
	    {error, timeout}
    end.

test() ->
    try 
	%% load v8
	ok = load(),

	%% context creation
	Context = context(),
	true = is_port(Context),

	%% type conversion
	{ok, true} = script_result(Context, <<"true;">>),
	{ok, false} = script_result(Context, <<"false;">>),
	{ok, null} = script_result(Context, <<"null;">>),
	{ok, undefined} = script_result(Context, <<"undefined;">>),
	{ok, undefined} = script_result(Context, <<"var foo = function() {};">>),
	{ok, []} = script_result(Context, <<"[];">>),
	{ok, [1, 2, 3]} = script_result(Context, <<"[1, 2, 3];">>),
	{ok, [1, true, 3, [4, false]]} = script_result(Context, <<"[1, true, 3, [4, false]];">>),
	{ok, {struct, ObjProps}} = script_result(Context, <<"var o = { 'foo': true, 'bar': false }; o;">>),
	2 = length(ObjProps),
	true = proplists:get_value(<<"foo">>, ObjProps),
	false = proplists:get_value(<<"bar">>, ObjProps),

	%% functions and context
	{ok, true} = script_result(Context, <<"var f = function() { return true; }; f();">>),
	{exception, {struct, ExProps}} = script_result(Context, <<"g();">>),
	<<"g is not defined">> = proplists:get_value(<<"message">>, ExProps),
	{ok, null} = script_result(Context, <<"var g = f; null;">>),
	{ok, true} = script_result(Context, <<"g();">>),

	%% context destruction
	true = destroy(Context),

	io:format("[v8]: all tests pass~n"),
	true
    catch
	T:E ->
	    io:format("[v8] test failure: ~w: ~w~n", [T, E]),
	    false
    end.
    

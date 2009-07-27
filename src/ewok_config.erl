%%
-module(ewok_config).
-vsn("1.0").
-author('steve@simulacity.com').

-include("ewok.hrl").

-export([load/0, load/1, unload/1, get/1, get/2]).
-export([print/0, print/1]).

%%
%% API
%%
load() ->
	%% Where should these be loaded????
	%  ewok_cache:add(ewok_wiki:get_routes()),
	load(ewok).
load(App) when is_atom(App) ->
	try begin
		case code:ensure_loaded(App) of
		{'module', App} ->
			Path = filename:dirname(code:which(App)),
			File = filename:join(Path, atom_to_list(App) ++ ?CONFIG_FILE_EXT),
			case filelib:is_regular(File) of
			true ->
				{ok, Terms} = file:consult(File),
				{ok, Config} = load_config(Terms, []),
				ewok_cache:add(Config);
			false ->
				{error, {nofile, File}}
			end;
		_ -> {error, {no_app_found, App}}
		end
	end catch
		Type:Reason -> {Type, Reason}
	end.
%
unload(App) when is_atom(App) ->
	not_implemented.

%
get(Key, Default) ->
	case ?MODULE:get(Key) of 
	undefined -> Default;
	Value -> Value
	end.
get(Key) when is_binary(Key) ->
	?MODULE:get(binary_to_list(Key));
get(Key) when is_list(Key) ->
	Parts = [list_to_atom(X) || X <- re:split(Key, "\\.", [{return, list}])],
	?MODULE:get(list_to_tuple(Parts));
get(Key) when is_tuple(Key) ->
	case ewok_cache:lookup(?MODULE, Key) of
	undefined -> undefined;
	{?MODULE, Key, Value} -> Value
	end.

%%
print() ->
	case ewok_cache:lookup(?MODULE) of
	undefined -> undefined;
	Recs -> 
		[io:format(" ~p=~p~n", [K, V]) || {?MODULE, K, V} <- Recs],
		{ok, length(Recs)}
	end.
%%	
print(Type) -> 
	case ewok_cache:lookup(Type) of
	undefined -> undefined;
	Recs -> 
		[io:format(" ~p~n", [X]) || X <- Recs],
		{ok, length(Recs)}
	end.

%%
%% internal
%%

%target format e.g.-> {ewok,server,ip}, {127,0,0,1}}
%% may later validate on type...
load_config([{_Type, Id, Props}|T], Acc) ->
	Config = load_properties({}, Id, Props),
	load_config(T, [Config|Acc]);
load_config([], Acc) ->
	{ok, lists:flatten(Acc)}.
%
load_properties(Parent, autodeploy, Props) ->
	Key = erlang:append_element(Parent, autodeploy),
	{?MODULE, Key, Props};
load_properties(Parent, roles, Props) ->
	Key = erlang:append_element(Parent, roles),
	{?MODULE, Key, Props};
load_properties(Parent, Id, Props) -> 
	Key = erlang:append_element(Parent, Id),
%	io:format("~p ~p~n", [Key, Props]),
	case is_list(Props) of 
	true -> 
		case proplists:get_keys(Props) of
		[] -> {?MODULE, Key, Props};
		Keys -> 
			F = fun (X) ->
				case proplists:get_value(X, Props) of
				undefined -> load_records(X, Props);
				Value -> load_properties(Key, X, Value)
				end
			end,
			[F(X) || X <- Keys]
		end;
	false -> {?MODULE, Key, Props}
	end.
%
load_records(Type, Props) ->
	[X || X = X1 <- Props, element(1, X1) =:= Type].

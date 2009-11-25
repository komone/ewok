%% Copyright 2009 Steve Davis <steve@simulacity.com>
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

%%
-module(ewok_configuration).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl").

-export([preload/1, load/1, unload/1, get_value/1, get_value/2]).
-export([print/0, print/1]).

%%
%% API
%
preload(App) when is_atom(App) ->
	case load_file(App) of
	{ok, File, Terms} -> {ok, File, parse_config(Terms, [])};
	Error -> Error
	end.
	
%
load(App) when is_atom(App) ->
	case preload(App) of
	{ok, _File, Config} -> ewok_cache:add(Config);
	Error -> Error
	end.
%
unload(App) when is_atom(App) ->
	case preload(App) of
	{ok, Config} -> ewok_cache:remove(Config);
	Error -> Error
	end.

%
get_value(Key, Default) ->
	case get_value(Key) of 
	undefined -> Default;
	Value -> Value
	end.
get_value(Key) when is_binary(Key) ->
	get_value(binary_to_list(Key));
get_value(Key) when ?is_string(Key) ->
	Parts = [list_to_atom(X) || X <- re:split(Key, "\\.", [{return, list}])],
	get_value(list_to_tuple(Parts));
get_value(Key) when is_tuple(Key) ->
	case ewok_cache:lookup(?MODULE, Key) of
	undefined -> undefined;
	{?MODULE, Key, Value} -> Value
	end.

%%
print() ->
	case ewok_cache:lookup(?MODULE) of
	undefined -> undefined;
	Recs -> 
		[io:format(" ~p = ~p~n", [K, V]) || {?MODULE, K, V} <- Recs],
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
%% Internal
%%

%%
load_file(App) ->
	case code:ensure_loaded(App) of
	{'module', App} ->
		Path = filename:dirname(code:which(App)),
		File = filename:join(Path, atom_to_list(App) ++ ?CONFIG_FILE_EXT),
		case filelib:is_regular(File) of
		true ->
			try
				{ok, Terms} = file:consult(File),
				{ok, File, Terms}
			catch
			%% C{error,{103,erl_parse,["syntax error before: ","'{'"]}}
			_:{badmatch, {error, {Line, _, Message}}} -> 
				{error, {file, File}, {line, Line}, {reason, lists:flatten(Message)}};
			Error:Reason -> 
				{Error, Reason}
			end;
		false ->
			{error, {nofile, File}}
		end;
	_ -> 
		{error, {no_app_found, App}}
	end.

%target format e.g.-> {ewok,server,ip}, {127,0,0,1}}
%% may later validate on Type...
parse_config([{_Type, Id, Props}|T], Acc) ->
	Config = parse_properties({}, Id, Props),
	parse_config(T, [Config|Acc]);
parse_config([], Acc) ->
	lists:flatten(Acc).
%
parse_properties(Parent, autodeploy, Props) ->
	Key = erlang:append_element(Parent, autodeploy),
	{?MODULE, Key, Props};
parse_properties(Parent, roles, Props) ->
	Key = erlang:append_element(Parent, roles),
	{?MODULE, Key, Props};
parse_properties(Parent, Id, Props) -> 
	Key = erlang:append_element(Parent, Id),
%	io:format("~p ~p~n", [Key, Props]),
	case is_list(Props) of 
	true -> 
		case proplists:get_keys(Props) of
		[] -> {?MODULE, Key, Props};
		Keys -> 
			F = fun (X) ->
				case proplists:get_value(X, Props) of
				undefined -> parse_records(X, Props);
				Value -> parse_properties(Key, X, Value)
				end
			end,
			[F(X) || X <- Keys]
		end;
	false -> {?MODULE, Key, Props}
	end.
%
parse_records(Type, Props) ->
	[X || X = X1 <- Props, element(1, X1) =:= Type].

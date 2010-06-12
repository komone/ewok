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

-module(ewok_config).

-include("ewok.hrl").
-include("ewok_system.hrl").

-export([lookup/1]).
-export([load/1, load/3, unload/1, values/0, get_value/1, get_value/2, put_value/2, get_env/1, get_env/2]).
-export([import_file/1, import_file/2, export_file/1]).
-export([print/0, print/1]).

%%
get_value(Property) ->
	lookup(Property).
%
lookup(Property) ->
	case ewok_db:lookup(?MODULE, Property) of
	#ewok_config{value = Value} ->
		Value;
	undefined ->
		undefined			
	end.
%
get_value(Property, Default) ->
	case ewok_db:lookup(?MODULE, Property) of
	#ewok_config{value = Value} -> 
		Value;
	undefined -> 	
		ok = ewok_db:create(#ewok_config{key = Property, value = Default}),
		Default
	end.
%	
put_value(Property, Value) ->
	ok = ewok_db:update(#ewok_config{key = Property, value = Value}).

%
get_env(Key) ->
	get_env(Key, undefined).
%
get_env(Key, Default) when is_atom(Key) ->
	case application:get_env(ewok, Key) of
	{ok, Value} -> 
		Value;
	undefined -> 
		Default
	end.
	
% 
values() ->
	{ok, Config} = ewok_db:select(?MODULE),
	[{X, Y} || {?MODULE, X, Y} <- Config].

%% TODO: This isn't right
load(App, Prefix, Terms) -> 
	%? application:unload(App),
	Config = parse_config([{{App}, Prefix, Terms}], []),
	ewok_db:add(Config).
%% TEMP
load([]) ->
	[].
%%
unload(_App) ->
	not_implemented.

%
export_file(File) ->
	{ok, Terms} = ewok_db:select(?MODULE),
	Tuples = lists:sort([{K, V} || {?MODULE, K, V} <- Terms]),
	%% Provisional
	{ok, Mimetypes} = ewok_db:select(ewok_mimetype),
	Tuples2 = lists:sort([{K, V} || {ewok_mimetype, K, V} <- Mimetypes]),
	{ok, Routes} = ewok_db:select(ewok_route),
	Tuples3 = lists:sort([{A, B, C, D} || {ewok_route, A, B, C, D} <- Routes]),

	%% TODO: now convert to a term and write the term out
	Path = ewok_file:path(File),
	ok = ewok_file:save(Path, io_lib:format("~p~n", [
		{ewok, [
			{config, Tuples},
			{mimetypes, Tuples2},
			{routes, Tuples3}
		]}
	])),
	{ok, Path}.
	
import_file(File) ->
	import_file(ewok, File).
import_file(_App, File) ->
	Path = ewok_file:path(File),
	case load_file(Path) of
	{ok, Terms} -> 
		Values = parse_config(Terms, []),
		ok = ewok_db:add(Values),
		{ok, Path, length(Values)};
	Error -> 
		Error
	end.

%%
print() ->
	case ewok_db:select(?MODULE) of
	{ok, Recs} -> 
		[io:format(" ~p = ~p~n", [K, V]) || {?MODULE, K, V} <- lists:sort(Recs)],
		{ok, length(Recs)};
	undefined -> 
		undefined
	end.
%%	
print(Type) -> 
	case ewok_db:select(Type) of
	{ok, Recs} -> 
		[io:format(" ~p~n", [X]) || X <- lists:sort(Recs)],
		{ok, length(Recs)};
	undefined -> 
		undefined
	end.

%%
%% Internal
%%

%%
load_file(Path) ->
	case ewok_file:is_regular(Path) of
	true ->
		try
			ewok_file:eval(Path)
		catch
		%% C{error,{103,erl_parse,["syntax error before: ","'{'"]}}
		_:{badmatch, {error, {Line, _, Message}}} -> 
			{error, {file, Path}, {line, Line}, {reason, lists:flatten(Message)}};
		Error:Reason -> 
			{Error, Reason}
		end;
	false ->
		{error, {nofile, Path}}
	end.

%target format e.g.-> {ewok,server,ip}, {127,0,0,1}}
%% may later validate on Type...
parse_config([{Parent, Id, Props}|T], Acc) ->
	Config = parse_properties(Parent, Id, Props),
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
	true when Props =:= [] ->
		{?MODULE, Key, Props};
	true ->
		case proplists:get_keys(Props) of
		[] -> {?MODULE, Key, list_to_binary(Props)};
		Keys -> 
			F = fun (X) ->
				case proplists:get_value(X, Props) of
				undefined -> 
					parse_records(X, Props);
				Value -> 
					parse_properties(Key, X, Value)
				end
			end,
			[F(X) || X <- Keys]
		end;
	false -> {?MODULE, Key, Props}
	end.
%%
parse_records(Type, Props) ->
	[convert_record(X) || X <- Props, element(1, X) =:= Type].
%
convert_record({route, default, Handler, Realm, Roles}) ->
	{ewok_route, default, Handler, Realm, Roles};
convert_record({route, Path, Handler, Realm, Roles}) ->
	{ewok_route, list_to_binary(Path), Handler, Realm, Roles};
convert_record({mimetype, default, Media}) ->
	{ewok_mimetype, default, list_to_binary(Media)};
convert_record({mimetype, Ext, Media}) ->
	{ewok_mimetype, list_to_binary(Ext), list_to_binary(Media)}.


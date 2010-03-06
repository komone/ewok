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

-module(ewok_config).

-include("ewok.hrl").
-include("ewok_system.hrl").

-export([load/3, unload/1, all/0, get_value/1, get_value/2, put_value/2, mimetype/1, get_env/1, get_env/2]).
-export([import_file/1, import_file/2, export_file/1, load_mimetypes/0]).
-export([print/0, print/1]).

-define(CONFIG, ?MODULE).

%%
%% API
%%

%% TODO: This isn't right
load(App, Prefix, Terms) -> 
	%? application:unload(App),
	Config = parse_config([{{App}, Prefix, Terms}], []),
	ewok_db:add(Config).

unload(_App) ->
	not_implemented.
	
%%
load_mimetypes() ->
	F = fun (X, Y) ->
		X1 = 
			case X of 
			_ when is_atom(X) -> X;
			_ -> list_to_binary(X)
			end, 
		Y1 = list_to_binary(Y),
		{ewok_mimetype, X1, Y1}
		end,
	Mimetypes = get_env(mimetypes, []),
	Records = [F(K, V) || {K, V} <- Mimetypes],
	ewok_db:add(Records),
	ewok_log:message(?MODULE, [Records]).

%%
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
all() ->
	{ok, Config} = ewok_db:select(?CONFIG),
	[{X, Y} || {?CONFIG, X, Y} <- Config].
%
get_value(Property) ->
	case ewok_db:lookup(?MODULE, Property) of
	undefined -> 
		undefined;
	#ewok_config{value = Value} -> 
		Value
	end.
%
get_value(Property, Default) ->
	case ewok_db:lookup(?CONFIG, Property) of
	undefined -> 	
		ok = ewok_db:create(#ewok_config{key = Property, value = Default}),
		Default;
	#ewok_config{value = Value} -> 
		Value
	end.
	
put_value(Property, Value) ->
	ok = ewok_db:update(#ewok_config{key = Property, value = Value}).
	
mimetype(Extension) ->
	case ewok_db:lookup(ewok_mimetype, Extension) of
	undefined -> 
		<<"application/octet-stream">>;
	{ewok_mimetype, Extension, Value} -> 
		Value
	end.

export_file(File) ->
	{ok, Terms} = ewok_db:select(?CONFIG),
	Tuples = lists:sort([{K, V} || {?CONFIG, K, V} <- Terms]),
	%% Provisional
	{ok, Mimetypes} = ewok_db:select(ewok_mimetype),
	Tuples2 = lists:sort([{K, V} || {ewok_mimetype, K, V} <- Mimetypes]),
	{ok, Routes} = ewok_db:select(ewok_route),
	Tuples3 = lists:sort([{A, B, C, D} || {ewok_route, A, B, C, D} <- Routes]),

	%% TODO: now convert to a term and write the term out
	ok = file:write_file(File, io_lib:format("~p~n", [
		{ewok, [
			{config, Tuples},
			{mimetypes, Tuples2},
			{routes, Tuples3}
		]}
	])),
	{ok, filename:absname(File)}.
	
import_file(File) ->
	import_file(ewok, File).
import_file(_App, File) ->
	case load_file(File) of
	{ok, Terms} -> 
		Values = parse_config(Terms, []),
		ok = ewok_db:add(Values),
		{ok, filename:absname(File), length(Values)};
	Error -> 
		Error
	end.

%%
print() ->
	case ewok_db:select(?CONFIG) of
	{ok, Recs} -> 
		[io:format(" ~p = ~p~n", [K, V]) || {?CONFIG, K, V} <- lists:sort(Recs)],
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
load_file(File) ->
	case filelib:is_regular(File) of
	true ->
		try
			file:consult(File)
		catch
		%% C{error,{103,erl_parse,["syntax error before: ","'{'"]}}
		_:{badmatch, {error, {Line, _, Message}}} -> 
			{error, {file, File}, {line, Line}, {reason, lists:flatten(Message)}};
		Error:Reason -> 
			{Error, Reason}
		end;
	false ->
		{error, {nofile, File}}
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
	{?CONFIG, Key, Props};
parse_properties(Parent, roles, Props) ->
	Key = erlang:append_element(Parent, roles),
	{?CONFIG, Key, Props};
parse_properties(Parent, Id, Props) -> 
	Key = erlang:append_element(Parent, Id),
%	io:format("~p ~p~n", [Key, Props]),
	case is_list(Props) of 
	true when Props =:= [] ->
		{?CONFIG, Key, Props};
	true ->
		case proplists:get_keys(Props) of
		[] -> {?CONFIG, Key, list_to_binary(Props)};
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
	false -> {?CONFIG, Key, Props}
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


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

-module(ewok_mime).

-include("ewok.hrl").

-compile(export_all).
-export([load/1, get_type/1]).

%% @def RFC 5322 <http://www.ietf.org/rfc/rfc5322.txt>

-define(DEFAULT_MIMETYPE, <<"application/octet-stream">>).

%% @def RFC2046, RFC2077
-define(TOP_LEVEL_TYPES, [
	<<"text">>, <<"image">>, <<"audio">>, <<"video">>, 
	<<"application">>, <<"multipart">>, <<"message">>,
	<<"model">>
]).

%% 
load(File) ->
	Path = ewok_file:path(File),
	Data = ewok_file:load(Path),
	parse(ewok_text:split(Data, <<"[\r\n]+">>), []).

%% Ignore comments
parse([<<$#, _/binary>>|T], Acc) ->
	parse(T, Acc);
%
parse([Line|T], Acc) ->
	Records = 
		case ewok_text:split(Line, <<"[ \t;]+">>) of
		[Mimetype | Extensions] ->
			case is_valid(Mimetype) of
			true ->
				% ?TTY({Mimetype, Extensions}),
				[{ewok_mimetype, X, Mimetype} || X <- Extensions];
			false ->
				?TTY({invalid, Mimetype, Extensions}),
				[]
			end;		
		Value ->
			?TTY({invalid, Value}),
			[]
		end,
	parse(T, lists:append(Records, Acc));
%
parse([], Acc) ->
	?TTY({loaded, length(Acc)}),
	ewok_db:add(Acc),
	ewok_log:message(?MODULE, [Acc]).

%%
is_valid(Mimetype) ->
	case ewok_text:split(Mimetype, <<"/">>) of
	[Type, _Subtype] ->
		lists:member(Type, ?TOP_LEVEL_TYPES);
	_ ->
		false
	end.
	
%% 
get_type(File) ->
	case ewok_file:extension(File) of
	<<$., Extension/binary>> ->
		case ewok_db:lookup(ewok_mimetype, Extension) of
		undefined -> 
			?DEFAULT_MIMETYPE;
		{ewok_mimetype, Extension, Value} -> 
			Value
		end;
	_ ->
		?DEFAULT_MIMETYPE
	end.
%%
load_mimetypes() ->
	F = fun (X, Y) ->
		X1 = 
			case X of 
			_ when is_atom(X) -> 
				X;
			_ -> 
				list_to_binary(X)
			end, 
		Y1 = list_to_binary(Y),
		{ewok_mimetype, X1, Y1}
		end,
	Mimetypes = ewok_util:get_env(mimetypes, []),
	Records = [F(K, V) || {K, V} <- Mimetypes],
	ewok_db:add(Records),
	ewok_log:message(?MODULE, [Records]).

%%
mimetype(Extension) ->
	case ewok_db:lookup(ewok_mimetype, Extension) of
	undefined -> 
		?DEFAULT_MIMETYPE;
	{ewok_mimetype, Extension, Value} -> 
		Value
	end.


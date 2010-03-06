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

-module(ewok_osmos).
-name("Ewok OSMOS Datasource").

-include("ewok.hrl").
-include("ewok_system.hrl").

-export([create/1, drop/1, save/3, find/2, info/0]).

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-record(state, {data_path, tables = []}).

%% TODO: extend to include range selects

%%
start_link(Args) ->
	osmos:start(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).
%%
stop() ->
    gen_server:cast(?SERVER, stop).
%%
create(Table) when is_atom(Table) ->
	gen_server:call(?SERVER, {open, Table}).
%%
drop(Table) when is_atom(Table) ->
	gen_server:call(?SERVER, {close, Table}).
%%
save(Table, Key, Value) ->
	gen_server:call(?SERVER, {write, Table, Key, Value}).
%%
find(Table, Key) ->
	gen_server:call(?SERVER, {read, Table, Key}).

%%
info() ->
	gen_server:call(?SERVER, info).

%%
%%% gen_server
%%
init(_Args) ->
	process_flag(trap_exit, true), % when do we need this?
	DataDir = ewok_config:get_value({ewok, data_dir}, ?DATA_DIR),
	DataPath = ewok_file:path([ewok_util:appdir(), DataDir, <<"osmos">>]),
    {ok, #state{data_path = DataPath}}.
%
handle_call({open, Table}, _From, State) ->
	case lists:member(Table, State#state.tables) of
	false ->
		Format = osmos_table_format:new(term, term_replace, 1024),
		Options = [{directory, binary_to_list(State#state.data_path)}, {format, Format}],
		Result = osmos:open(Table, Options),
		NewState = 
			case Result of
			{ok, Table} ->
				State#state{tables = [Table|State#state.tables]};
			_ ->
				ewok_log:error("ewok_osmos: Unable to create table \'" 
					++ ewok_text:value(Table) ++ "\'"),
				State
			end;
	true ->
		Result = ok,
		NewState = State
	end,
	{reply, Result, NewState};
%
handle_call({close, Table}, _From, State) ->
	case lists:member(Table, State#state.tables) of
	true ->
		Result = osmos:close(Table),
		NewState = 
			case Result of
			ok ->
				Tables = lists:delete(Table, State#state.tables),
				State#state{tables = Tables};
			_ ->
				ewok_log:error("ewok_osmos: Unable to delete table \'" 
					++ ewok_text:value(Table) ++ "\'"),
				State
			end;
	false ->
		Result = undefined,
		NewState = State
	end,
	{reply, Result, NewState};
%
handle_call({write, Table, Key, Value}, _From, State) ->
	Result = 
		case lists:member(Table, State#state.tables) of
		true ->
			osmos:write(Table, Key, Value);
		false ->
			undefined
		end,
	{reply, Result, State};
%
handle_call({read, Table, Key}, _From, State) ->
	Result = 
		case lists:member(Table, State#state.tables) of
		true ->
			case osmos:read(Table, Key) of
			{ok, Value} ->
				{ok, Value};
			not_found ->
				undefined
			end;
		false ->
			undefined
		end,
	{reply, Result, State};
%
handle_call(info, _From, State) ->
	{reply, {tables, State#state.tables}, State};
	
handle_call(_Message, _From, State) ->
	{reply, not_implemented, State}.
%%
handle_cast(stop, State) ->
    {stop, normal, State};
%
handle_cast(_, State) ->
    {noreply, State}.
%%
handle_info(_Info, State) ->
    {noreply, State}.
%%
terminate(_Reason, State) ->
	[osmos:close(Table) || Table <- State#state.tables],
    ok.
%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.



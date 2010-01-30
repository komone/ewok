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

-module(ewok_logging_srv).
-name("Ewok Logging Service").

-include("ewok.hrl").
-include("ewok_system.hrl").
-include_lib("kernel/include/file.hrl").

%% *** NOTE: This entire module needs a thorough review...!!!
%% *** NOTE: *** Consider using kernel's disk_log module instead

%% *** NOTE: need to implement a scheduled task to rollover the log files...
%% *** NOTE: Later, it will probably be better to add 'delayed_write' as an
%% option for file:open (currently at lines 194, 226) - at least in production mode

-behaviour(ewok_service). %%?
-export([start_link/0, stop/0]).

-behaviour(gen_event).
-export([code_change/3, handle_call/2, handle_event/2, handle_info/2, init/1, terminate/2]).

-record(state, {id, fd, log_dir = [], level = info, logs = [], prev}).

%% TEMP
-compile(export_all).

%%
start_link() ->
    application:load(ewok),
    LogDir = ewok_util:get_env(log_dir, ?LOG_DIR),
    BootLog = ewok_util:get_env(boot_log, ?BOOT_LOG),
    case lists:member(?MODULE, gen_event:which_handlers(error_logger)) of
	false ->
		ok = gen_event:add_sup_handler(error_logger, ?MODULE, {LogDir, BootLog});
	true -> 
		ok
    end.

%%
stop() -> 
	error_logger:delete_report_handler(?MODULE).

%%
level(debug) -> 4;
level(info)  -> 3;
level(warn)  -> 2;
level(error) -> 1;
level(fatal) -> 0;
level(_)     -> level(info).

%%
%% gen_event callbacks
%%
init({{LogDir, BootLog}, {Prev, Buf}}) ->
    process_flag(trap_exit, true),
    RootLogPath = ewok_file:path([LogDir]),
    BootLogPath = ewok_file:path([ewok_util:appdir(), BootLog]),
    case ewok_file:open(BootLogPath, [raw, write]) of
	{ok, Fd} ->
		case Prev of
	    undefined -> 
			ok;
	    _ -> 
			write_messages(Fd, Prev, Buf)
		end,
		BootLogRecord = #ewok_log{id = boot, fd = Fd, path = BootLogPath},
		{ok, #state{id = boot, fd = Fd, log_dir = RootLogPath, logs = [BootLogRecord], prev = Prev}};
	Error -> Error
    end;
init(Opts = {_, _}) -> 
	init({Opts, {undefined, []}}).

% {error, Gleader, {Pid, Format, Data}}
% {error_report, Gleader, {Pid, std_error, Report}}
% {error_report, Gleader, {Pid, Type, Report}}
%%
handle_event({_, GL, _}, State)
    when node(GL) =/= node() ->
    ?TTY({handle_event, node(GL)}), 
	{ok, State};
handle_event({_, _GL, {_From, ?MODULE, {default, Type, Messages}}}, State) ->
    case level(Type) =< State#state.level of
	false -> 
		ignore;
	true ->
%		Formatted = list_to_binary(io_lib:format("~p", [Messages])),
		write_messages(State#state.fd, Type, Messages)
    end,
    {ok, State};
handle_event({_, _GL, {_From, ?MODULE, {LogId, Type, Messages}}}, State) ->
    case lists:keyfind(LogId, 2, State#state.logs) of
	false -> 
		ignore;
	Log -> 
		write_messages(Log#ewok_log.fd, Type, Messages)
    end,
    {ok, State};
handle_event({_, _GL, {_From, crash_report, Messages}}, State) ->
    StackTrace = list_to_binary(io_lib:format("~n~p~n", [Messages])),
    write_messages(State#state.fd, error, StackTrace),
    {ok, State};
handle_event({_, _GL, {_From, progress, [{supervisor, {Dist, Process}} | Messages]}}, State) ->
    StackTrace = list_to_binary(io_lib:format("~p", [[Dist | Messages]])),
    write_messages(State#state.fd, Process, StackTrace),
    {ok, State};
handle_event({_, _GL, {_From, Other, Messages}}, State) ->
    StackTrace = list_to_binary(io_lib:format("~p", [Messages])),
    write_messages(State#state.fd, Other, StackTrace),
    {ok, State};
handle_event(Message, State) ->
    ?TTY({handle_event, ?MODULE, Message}), 
	{ok, State}.

%%
handle_call({set_default_log, Id}, State) ->
    %	?TTY("SET DEFAULT LOG~n", []),
    {Reply, NewState} = 
		case lists:keyfind(Id, 2, State#state.logs) of
		#ewok_log{fd = Fd, id = Id1, path = Path} ->
			write_messages(State#state.fd, ?MODULE, [{continues, binary_to_list(Path)}]),
			{ok, State#state{id = Id1, fd = Fd}};
		false -> {{error, invalid_id}, State}
		end,
    {ok, Reply, NewState};
%%
handle_call({add_log, Id}, State) ->
    {Reply, NewState} = 
		case lists:keyfind(Id, 2, State#state.logs) of
		#ewok_log{} -> 
			{exists, State};
		false ->
			%% TODO: What to do with these two configurations...
			Rollover = infinity,
			MaxFiles = infinity,
			case create_log(State#state.log_dir, Id, Rollover, MaxFiles) of
			{ok, #ewok_log{} = Log} ->
				{ok, State#state{logs =[Log | State#state.logs]}};
			Other -> {Other, State}
			end
		end,
    {ok, Reply, NewState};
%
handle_call({remove_log, Id}, State) ->
    {Reply, NewState} = 
		case lists:keyfind(Id, 2, State#state.logs) of
		false -> 
			{{error, undefined}, State};
		#ewok_log{id = Id1} when Id1 =:= State#state.id ->
			{{error, default_log}, State};
		Log ->
			write_messages(State#state.fd, ?MODULE, [{Log#ewok_log.id, closed}]),
			file:close(Log#ewok_log.fd),
			{ok, State#state{logs = lists:keydelete(Id, 2, State#state.logs)}}
		end,
    {ok, Reply, NewState};
%
handle_call({rollover, Id}, State) ->
    {Reply, NewState} = 
		case lists:keyfind(Id, 2, State#state.logs) of
		false -> 
			{{error, no_file}, State};
		Log ->
			write_messages(Log#ewok_log.fd, ?MODULE, [{Log#ewok_log.id, closed}]),
			ok = file:close(Log#ewok_log.fd),
			Success = archive_file(Log#ewok_log.path),
			{ok, Fd} = file:open(Log#ewok_log.path, [raw, append]), %? delayed_write]),
			write_messages(Fd, ?MODULE, [{rollover, Id, Success}]),
			LogList = lists:keystore(Id, 2, State#state.logs, Log#ewok_log{fd = Fd}),
			case Id =:= State#state.id of
			true ->
				{ok, State#state{fd = Fd, logs = LogList}};
			false -> 
				{ok, State#state{logs = LogList}}
			end
		end,
    {ok, Reply, NewState};
%
handle_call({log_info, all}, State) ->
    {ok, {ok, State#state.logs}, State};
handle_call({log_info, default}, State) ->
    LogInfo = 
		case lists:keyfind(State#state.id, 2, State#state.logs) of
		false -> 	
			undefined;
		Value -> 
			{ok, Value}
		end,
    {ok, LogInfo, State};
handle_call({log_info, Id}, State) ->
    LogInfo = 
		case lists:keyfind(Id, 2, State#state.logs) of
		false -> 
			undefined;
		Value -> 
			{ok, Value}
		end,
    {ok, LogInfo, State};
%
handle_call(Call, State) ->
    ?TTY({handle_call, Call}),
    {ok, {error, not_supported}, State}.

%%
handle_info(Message = {'EXIT', _Fd, _Reason}, State) ->
    ?TTY({handle_info, Message}),
    case State#state.prev of
      undefined -> 
		remove_handler;
      Prev ->
		{swap_handler, install_prev, [Message], Prev, go_back}
    end;
handle_info({emulator, GL, Chars}, State)
    when node(GL) =:= node() ->
    write_messages(State#state.fd, group_leader, Chars),
    {ok, State};
handle_info({emulator, noproc, Chars}, State) ->
    write_messages(State#state.fd, noproc, Chars),
    {ok, State};
handle_info(Info, State) ->
    ?TTY({handle_info, Info, State}), 
	{ok, State}.

%% Is State here ever not #state?
%% Sbould this install previous (like in 'EXIT')?
terminate(Reason, #state{logs = Logs, fd = Fd}) ->
    write_messages(Fd, shutdown, [Reason]),
    [file:close(Log#ewok_log.fd) || Log <- Logs],
    ok.

%%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.

%%
%% Internal
%%
%%
create_log(Path, Id, Rollover, MaxFiles) ->
    File = ewok_file:path([Path, atom_to_list(Id) ++ (?LOG_FILE_EXT)]),
    case ewok_file:file_info(File) of
	{ok, FileInfo} ->
		{DateNow, _} = calendar:universal_time(),
		[{DateThen, _}] = calendar:local_time_to_universal_time_dst(FileInfo#file_info.mtime),
		case DateNow =/= DateThen of
	    true ->
			archive_file(File); %% TODO: log the result of this...
	    false -> 
			ok
		end;
      _ ->
		ok  %% if we can't access the old log then.... don't worry about it?
    end,
    %% NOTE: !! Later, it will probably be better to add 'delayed_write' as an option for file:open
    case ewok_file:open(File, [raw, append]) of
	{ok, Fd} ->
		{ok, #ewok_log{id = Id, fd = Fd, path = File, rollover = Rollover, maxfiles = MaxFiles}};
	Error -> 
		Error
    end.

%%
archive_file(Path) ->
    Timestamp = file_time(erlang:universaltime(), "Z"),
    Archive = ewok_file:path([<<Path/binary, $-, Timestamp/binary, (?LOG_ARCHIVE_EXT)/binary>>]),
    Result = erl_tar:create(binary_to_list(Archive), [binary_to_list(Path)], [compressed]),
    {Result, Archive}.

file_time({{Year, Month, Day}, {Hour, Min, Sec}}, Zone) ->
    Format = "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0B~s",
    list_to_binary(io_lib:format(Format, [Year, Month, Day, Hour, Min, Sec, Zone])).

%% This should now be much more robust...
write_messages(Fd, Type, Message) when is_binary(Message) ->
    write_message(Fd, Type, Message);
write_messages(Fd, Type, String) when ?is_string(String) ->
    write_message(Fd, Type, String);
write_messages(Fd, Type, [H | T]) ->
    write_message(Fd, Type, H), 
	write_messages(Fd, Type, T);
write_messages(_, _, []) -> 
	ok.

%%
write_message(Fd, Type, Message) -> %%
    Tag = ewok_util:to_binary(Type),
    Line = ewok_util:to_binary(Message),
    % HACK placeholder
    Line1 = re:replace(Line, "[ \t\r\n]+", " ", [global]),
    Log = list_to_binary([ewok_util:timestamp(), <<" [">>, Tag, <<"] ">>, Line1, <<$\n>>]),
    %	io:format(user, Log, []),
    file:write(Fd, Log).

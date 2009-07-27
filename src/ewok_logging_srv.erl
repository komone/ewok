%%
%%
-module(ewok_logging_srv).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("../include/ewok.hrl").
-include("ewok_system.hrl").
-include_lib("kernel/include/file.hrl").

%% *** NOTE: This entire module needs a thorough review...!!!
%% *** NOTE: In order for this to be controlled by ewok_sup, we need
%% to change error_logger:add_report_handler to gen_event:add_sup_handler().
%% this should be done carefully to ensure all dependency requirements are
%% met.
%% *** NOTE: need to implement a scheduled task to rollover the log files...
%% *** NOTE: Later, it will probably be better to add 'delayed_write' as an 
%% option for file:open (currently at lines 194, 226) - at least in production mode

-behavior(ewok_service).
-export([start_link/0, stop/0, service_info/0]).

-behaviour(gen_event).
-export([init/1, handle_event/2, handle_call/2, 
	handle_info/2, terminate/2, code_change/3]).

-record(state, {id, fd, level=info, logs=[], prev}).

%% 
start_link() ->	
	case lists:member(?MODULE, gen_event:which_handlers(error_logger)) of
	false ->
		application:load(ewok),
		{ok, Opts} = application:get_env(ewok, boot),
		LogId = proplists:get_value(log, Opts, boot),
		LogPath = proplists:get_value(path, Opts, "."),
		Overwrite = proplists:get_value(rollover, Opts, false),
		gen_event:add_sup_handler(error_logger, ?MODULE, {LogId, LogPath, Overwrite});
	true -> 
		ok
	end.

%%
stop() ->
	error_logger:delete_report_handler(?MODULE).

%%
service_info() -> [ 
	{name, "Ewok Logging Service"},
	{version, {1,0,0}},
	{comment, ""}
].

%%
level(debug) -> 4;
level(info)  -> 3;
level(warn ) -> 2;
level(error) -> 1;
level(fatal) -> 0;
level(_)     -> level(info).

%%
%% gen_event callbacks
%%
init({{Id, Path, Overwrite}, {Prev, Buf}}) ->
    process_flag(trap_exit, true),
	File = to_log_filename(Path, Id),
	case Overwrite =:= false andalso filelib:is_regular(File) of
	true ->
		ArchiveFile = to_archive_filename(File),
		file:rename(File, ArchiveFile);
	false -> 
		ok
	end,
	case file:open(File, [raw, write]) of
	{ok, Fd} ->
		case Prev of
		undefined -> ok;
		_ -> write_messages(Fd, Prev, Buf)
		end,
		BootLog = #log{id=Id, fd=Fd, path=File},
		{ok, #state{id=Id, fd=Fd, logs=[BootLog], prev=Prev}};
	Error -> Error
    end;
init(Opts = {_, _, _}) ->
	init({Opts, {undefined, []}}).

% {error, Gleader, {Pid, Format, Data}}
% {error_report, Gleader, {Pid, std_error, Report}}
% {error_report, Gleader, {Pid, Type, Report}}
%%
handle_event({_, GL, _}, State) when node(GL) =/= node() ->
    {ok, State};
handle_event({_, _GL, {_From, ?MODULE, {default, Type, Messages}}}, State) ->
	case level(Type) =< State#state.level of
	false -> ignore;
	true -> write_messages(State#state.fd, Type, Messages)
	end,
    {ok, State};
handle_event({_, _GL, {_From, ?MODULE, {LogId, Type, Messages}}}, State) ->
	case lists:keyfind(LogId, 2, State#state.logs) of
	false -> ignore;
	Log -> write_messages(Log#log.fd, Type, Messages)
	end,
    {ok, State};
handle_event({_, _GL, {_From, crash_report, Messages}}, State) ->
	StackTrace = list_to_binary(io_lib:format("~n~p~n", [Messages])),
	write_messages(State#state.fd, error, StackTrace),
    {ok, State};
handle_event({_, _GL, {_From, Other, Messages}}, State) ->
	StackTrace = list_to_binary(io_lib:format("~p", [Messages])),
	write_messages(State#state.fd, Other, StackTrace),
    {ok, State};
handle_event(Message, State) ->
	?TTY("EVENT ~p ~p~n", [?MODULE, Message]),
    {ok, State}.
%%
handle_call({set_default_log, Id}, State) ->
	{Reply, NewState} = 
		case lists:keyfind(Id, 2, State#state.logs) of
		Log when is_record(Log, log) ->
			write_messages(State#state.fd, ?MODULE, [{continues, Log#log.path}]),
			{ok, State#state{id=Log#log.id, fd=Log#log.fd}};
		false -> 
			{{error, invalid_id}, State}
		end,
	{ok, Reply, NewState};
%%
handle_call({add_log, Id}, State) ->
	{Reply, NewState} = 
		case lists:keyfind(Id, 2, State#state.logs) of
		Log when is_record(Log, log) -> 
			{exists, State};
		false -> 
			case create_log(Id) of 
			{ok, Log} when is_record(Log, log) ->
				{ok, State#state{logs=[Log|State#state.logs]}};
			Other ->
				{Other, State}
			end
		end,
	{ok, Reply, NewState};
%
handle_call({remove_log, Id}, State) ->
	{Reply, NewState} = 
		case lists:keyfind(Id, 2, State#state.logs) of
		false -> 
			{{error, undefined}, State};
		Log when Log#log.id =:= State#state.id ->
			{{error, default_log}, State};
		Log ->
			write_messages(State#state.fd, ?MODULE, [{Log#log.id, closed}]),
			file:close(Log#log.fd),
			{ok, State#state{logs=lists:keydelete(Id, 2, State#state.logs)}}
		end,
	{ok, Reply, NewState};
%
handle_call({rollover, Id}, State) ->
	{Reply, NewState} = 
		case lists:keyfind(Id, 2, State#state.logs) of
		false -> 
			{{error, no_file}, State};
		Log ->
			ok = file:close(Log#log.fd),
			ArchiveFile = to_archive_filename(Log#log.path),
			Success = file:rename(Log#log.path, ArchiveFile),
			{ok, Fd} = file:open(Log#log.path, [raw, append]), %? delayed_write]),
			Message = lists:flatten(io_lib:format("~p, ~p, ~p", [Id, ArchiveFile, Success])),
			write_messages(Fd, ?MODULE, [{rollover, Message}]),
			LogList = lists:keystore(Id, 2, State#state.logs, Log#log{fd=Fd}),
			case Id =:= State#state.id of
			true ->
				{ok, State#state{fd=Fd, logs=LogList}};
			false ->
				{ok, State#state{logs=LogList}}
			end
		end,
	{ok, Reply, NewState};
%
handle_call({log_info, all}, State) ->
	{ok, State#state.logs, State};
handle_call({log_info, Id}, State) ->
	LogInfo = 
		case lists:keyfind(Id, 2, State#state.logs) of
		false -> undefined;
		Value -> {ok, Value}
		end,
    {ok, LogInfo, State};
%
handle_call(Call, State) ->
	?TTY("CALL ~p ~p~n", [?MODULE, Call]),
    {ok, {error, not_supported}, State}.

%%
handle_info(Message = {'EXIT', _Fd, _Reason}, State) ->
	?TTY("~p~n", [Message]),
    case State#state.prev of
	undefined -> remove_handler;
	Prev -> {swap_handler, install_prev, [Message], Prev, go_back}
    end;
handle_info({emulator, GL, Chars}, State) when node(GL) == node() ->
    write_messages(State#state.fd, group_leader, Chars),
    {ok, State};
handle_info({emulator, noproc, Chars}, State) ->
    write_messages(State#state.fd, noproc, Chars),
    {ok, State};
handle_info(Info, State) ->
	?TTY("INFO: ~p~n", [{Info, State}]),
    {ok, State}.
%% Is State here ever not #state?
%% Sbould this install previous (like in 'EXIT')?
terminate(_Reason, _State = #state{logs=Logs}) ->
%	?TTY("TERMINATE: ~p~n", [{Reason, State}]),
	[file:close(Log#log.fd) || Log <- Logs],
	ok.
%%
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%
%% Internal 
%%
%%
create_log(Id) ->
	case ewok_config:get({ewok, log, Id, enable}, false) of
	true ->
		File = to_log_filename(Id),
		case file:read_file_info(File) of
		{ok, FileInfo} ->
			{DateNow, _} = calendar:universal_time(),
			[{DateThen, _}] = calendar:local_time_to_universal_time_dst(FileInfo#file_info.mtime),
			case DateNow =/= DateThen of
			true ->
				ArchiveFile = to_archive_filename(File),
				file:rename(File, ArchiveFile);
			false -> 
				ok
			end;
		_ ->
			ok  %% if we can't access the old log then.... don't worry about it?
		end,
		Rollover = ewok_config:get({ewok, log, Id, rollover}, infinity),
		MaxFiles = ewok_config:get({ewok, log, Id, maxfiles}, infinity),
	%% NOTE: !! Later, it will probably be better to add 'delayed_write' as an option for file:open
		case file:open(File, [raw, append]) of
		{ok, Fd} -> 
			{ok, #log{id=Id, fd=Fd, path=File, rollover=Rollover, maxfiles=MaxFiles}};
		Error -> 
			Error
		end;
	false -> not_enabled
	end.

%%
to_log_filename(Path, Id) ->
	Filename = atom_to_list(Id) ++ ?LOG_FILE_EXT, 
	AppPath = ewok_config:get("ewok.log.path", ewok_util:appdir()),
	filename:absname(filename:join([AppPath, Path, Filename])).
	
to_log_filename(Id) ->
	Filename = atom_to_list(Id) ++ ?LOG_FILE_EXT, 
	Path = ewok_config:get("ewok.log.path", ewok_util:appdir()),
	filename:absname(filename:join(Path, Filename)).
%%
to_archive_filename(Path) ->
	{{Y, Mo, D}, {H, M, S}} = calendar:universal_time(),
	Dir = filename:dirname(Path),
	BaseName = filename:basename(Path, ?LOG_FILE_EXT),
	DateFormat = "~4.10.0B~2.10.0B~2.10.0BT~2.10.0B~2.10.0B~2.10.0B~s",
	FileDate = io_lib:format(DateFormat, [Y, Mo, D, H, M, S, "Z"]),
	ArchiveFile = lists:flatten([BaseName, "-", FileDate, ?LOG_FILE_EXT]),
	filename:join(Dir, ArchiveFile).

%% This should now be much more robust...
write_messages(Fd, Type, Message) when is_binary(Message) ->
	write_message(Fd, Type, Message);
write_messages(Fd, Type, String) when ?is_string(String) ->
	write_message(Fd, Type, String);
write_messages(Fd, Type, [H|T]) ->
	write_message(Fd, Type, H),
	write_messages(Fd, Type, T); 
write_messages(_, _, []) ->
	ok.

%%
write_message(Fd, Type, Message) -> %%
	Tag =
		case Type of
		_ when is_atom(Type) -> atom_to_binary(Type, utf8);
		_ when is_binary(Type) -> Type;
		_ when ?is_string(Type) -> list_to_binary(Type)
		end,
	Line = 
		case Message of
		_ when is_binary(Message) ->
			Message;
		_ when ?is_string(Message) -> 
			list_to_binary(Message);			
		_ -> %% THIS LINE IS A HORRIBLE HACK...
			list_to_binary(re:replace(io_lib:format("~p", [Message]), "[ \t\r\n]+", " ", [global]))
		end,
	Log = list_to_binary([ewok_util:timestamp(), <<" [">>, Tag, <<"] ">>, Line, <<$\n>>]),
%	io:format(user, Log, []),
	file:write(Fd, Log).

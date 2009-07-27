%%
%%
-module(ewok_log).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-export([add_log/1, remove_log/1, set_default/1, 
	rollover/1, log_info/0, log_info/1, debug/1, 
	info/1, warn/1, error/1, fatal/1, log/2, log/3]).

-define(LOGGER, ewok_logging_srv).

%%
add_log(Id) when is_atom(Id) ->
	gen_event:call(error_logger, ?LOGGER, {add_log, Id}).

%%
remove_log(Id) when is_atom(Id) ->
	gen_event:call(error_logger, ?LOGGER, {remove_log, Id}).

%%
set_default(Id) when is_atom(Id) ->
	gen_event:call(error_logger, ?LOGGER, {set_default_log, Id}).

%%
rollover(Id) when is_atom(Id) ->
	gen_event:call(error_logger, ?LOGGER, {rollover, Id}).

%%
log_info() ->
	log_info(all).
log_info(Id) when is_atom(Id) ->
	gen_event:call(error_logger, ?LOGGER, {log_info, Id}).

%%
debug(Message) -> log({default, debug, Message}).
info(Message)  -> log({default, info, Message}).
warn(Message)  -> log({default, warn, Message}).
error(Message) -> log({default, error, Message}).
fatal(Message) -> log({default, fatal, Message}).

%% Use error_report for everything and manage log_level internally
%% Using format irresponsibly can crash out the error_logger entirely,
%% so force custom formatting work into the appropriate service and not 
%% try to do it here...
log(Report = {LogId, _, Message}) when is_atom(LogId), is_list(Message) ->
	Return = error_logger:error_report(?LOGGER, Report),
	Return;
log({LogId, Type, Message}) ->
	log({LogId, Type, [Message]}).
%%
log(LogId, Message) ->	log({LogId, undefined, Message}).
%%
log(LogId, Level, Message) -> log({LogId, Level, Message}).


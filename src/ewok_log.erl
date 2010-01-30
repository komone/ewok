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

-module(ewok_log).

-include("ewok.hrl").

-export([add_log/1, remove_log/1, set_default/1, 
	rollover/1, log_info/0, log_info/1, message/2, message/3,
	debug/1, info/1, warn/1, error/1, fatal/1]).

-define(LOGGER, ewok_logging_srv).

%%
message(Tag, Message) -> 
	log({default, Tag, Message}).
message(Log, Tag, Message) when is_atom(Log) -> 
	log({Log, Tag, Message}).
%%
debug(Message) -> log({default, debug, Message}).
info(Message)  -> log({default, info, Message}).
warn(Message)  -> log({default, warn, Message}).
error(Message) -> log({default, error, Message}).
fatal(Message) -> log({default, fatal, Message}).

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

%% Use error_report for everything and manage log_level internally
%% Using format irresponsibly can crash out the error_logger entirely,
%% so force custom formatting work into the appropriate service and not 
%% try to do it here...
log(Report = {LogId, _, Message}) when is_atom(LogId), is_list(Message) ->
	error_logger:error_report(?LOGGER, Report);
log({LogId, Type, Message}) ->
	log({LogId, Type, [Message]}).

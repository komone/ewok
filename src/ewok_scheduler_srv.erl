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

-module(ewok_scheduler_srv).
-name("Ewok Scheduler Service").

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_service).
-export([start_link/0, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).
%% API
-export([add_task/1, cancel_task/1, get_tasks/0]).

-define(SERVER, ?MODULE).
-define(ETS, ?MODULE).

-record(state, {data=[]}).

%%
%% API
%% 
add_task(Task) when is_record(Task, ewok_task) ->
	gen_server:call(?SERVER, {add_task, Task}, infinity).

%%
cancel_task(Task) when is_record(Task, ewok_task) ->
	cancel_task(Task#ewok_task.id);
cancel_task(TaskId) when is_atom(TaskId) ->
	gen_server:call(?SERVER, {cancel_task, TaskId}, infinity). 

%%
get_tasks() ->
	gen_server:call(?SERVER, all_tasks, infinity). 

%%
%% ewok_service callbacks
%%
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).
stop() ->
    gen_server:cast(?SERVER, stop).

%%
%% gen_server callbacks
%%
init([]) ->
    ets:new(?ETS, [set, named_table, private, {keypos, 2}]),
	State = #state{},
    {ok, State}.

%%
handle_call({add_task, T}, _From, State) ->
	Reply = 
		case ets:member(?ETS, T#ewok_task.id) of
		false ->
			T1 = convert_times(T),
			schedule(T1);
		true -> 
			{error, id_in_use}
		end,
	{reply, Reply, State};
%% NOTE: cancel_task should maybe be a cast?
handle_call({cancel_task, TaskId}, _From, State) -> 
	Reply = 
		case ets:lookup(?ETS, TaskId) of
		[Task] -> 
			erlang:cancel_timer(Task#ewok_task.timer_ref),
			ets:delete(?ETS, Task#ewok_task.id),
			ok;
		_ -> ok
		end,
    {reply, Reply, State};
handle_call(all_tasks, _From, State) ->
	{reply, ets:tab2list(?ETS), State}.

%%
handle_cast(stop, State) ->
    {stop, normal, State};
%
handle_cast(_Msg, State) ->
    {noreply, State}.

%%
handle_info({exec, TaskId}, State) ->
	Now = unow(),
	case ets:lookup(?ETS, TaskId) of
	[Task] ->
		case exec_task(Task, Now) of
		{ok, Result} ->
			notify(task_executed, Now, Task, Result),
			case schedule(Task) of
			T when is_record(T, ewok_task) -> T;
			Reason ->
				ets:delete(?ETS, Task#ewok_task.id),
				notify(task_terminated, Now, Task, Reason)
			end;
		Error ->
			notify(task_failed, Now, Task, Error),
			schedule(Task)
		end;
	_ -> error %% LOG THIS
	end,
    {noreply, State}.

%%
terminate(_Reason, _State) ->
    ok.
	
%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.	

%%
%% Internal functions
%%

%% atom(), integer() -> TimerRef()
schedule(T) when is_record(T, ewok_task) ->
	Now = unow(),
	case calc_next(Now, T#ewok_task.start, T#ewok_task.repeat, T#ewok_task.terminate) of
	Seconds when is_integer(Seconds) ->
		TimerRef = erlang:send_after(Seconds * 1000, self(), {exec, T#ewok_task.id}),
		NextTask = T#ewok_task{timer_ref=TimerRef},
		ets:insert(?ETS, NextTask), %% Update
		NextTask;
	Message -> Message
	end.
%
	
%% from ewok.hrl
%-record(ewok_task, {id, function, start=now, repeat=once, terminate=undefined, notify, timer_ref}).

%% IMPL: This is rather dense validation code! Be very careful when making changes.
%% It aims to catch all invalid schedules at first schedule time and also detect normal exits.
calc_next(_, _, _, End)
		when is_atom(End) andalso End =/= infinity ->
	{error, invalid_schedule};		
%% asserts that Interval is a positive integer
calc_next(_, _, Interval, _) 
		when is_integer(Interval) andalso Interval =< 0 ->
	{error, invalid_schedule};
%% asserts that End is after Start
calc_next(_, Start, _, End)
		when is_integer(Start) andalso is_integer(End)
		andalso Start > End ->
	{error, invalid_schedule};
%% asserts that at least one execution will occur
calc_next(_, Start, Interval, End)
		when is_integer(Interval) andalso Interval > End - Start ->
	{error, invalid_schedule};
%% asserts and processes valid scheduling cases
calc_next(Now, Start, _, infinity) 
		when is_integer(Start) andalso Start > Now -> 
	Start - Now;
calc_next(Now, Start, _, End)
		when is_integer(Start) andalso is_integer(End) 
		andalso Start > Now andalso Start =< End ->
	Start - Now;
calc_next(_, _, Interval, infinity) 
		when is_integer(Interval) andalso Interval > 0 ->
	Interval;
calc_next(_, _, Interval, infinity) 
		when is_integer(Interval) andalso Interval > 0 ->
	Interval;
calc_next(Now, _, Interval, End)
		when  is_integer(Interval) andalso is_integer(End)
		andalso Interval > 0 andalso Now + Interval =< End ->
	Interval;
%% asserts that the task should be executed again at some time in the future
calc_next(_, _, once, _) ->
	{ok, complete};
%% asserts that there are more executions before the end of the schedule
calc_next(Now, _, Interval, End)
		when is_integer(Interval) andalso is_integer(End)
		andalso Now + Interval > End ->
	{ok, complete};
%% asserts that we are at or after the end of the schedule
calc_next(Now, _, _, End) 
	when is_integer(End) andalso End =< Now ->
	{ok, complete};
%% asserts that anything else must be wrong
calc_next(_, _, _, _) ->
	{error, invalid_schedule}.

%
exec_task(Task, Now) ->
	F = Task#ewok_task.function,
	try begin
		Result = F(Now),
		{ok, Result}
	end catch
	Error:Reason ->
		{Error, Reason}
	end.
%
notify(Message, Time, Task, Result) ->
    case Task#ewok_task.notify of
	Pid when is_pid(Pid) -> 
		case is_process_alive(Pid) of
		true -> Pid ! {?SERVER, Message, Time, Task#ewok_task.id, Result};
		false -> ok %% also remove notify from ewok_task record?
		end;
	_ -> ok
    end.

%%
unow() ->
    calendar:datetime_to_gregorian_seconds(calendar:universal_time()).
%%
convert_times(T) when is_record(T, ewok_task) ->
	T#ewok_task{
		start=convert_time(T#ewok_task.start),
		terminate=convert_time(T#ewok_task.terminate)
	}.
%%
convert_time(T) when is_integer(T) -> T;
convert_time(T = {{_,_,_},{_,_,_}}) ->
	[UTC] = calendar:local_time_to_universal_time_dst(T),
	calendar:datetime_to_gregorian_seconds(UTC);
convert_time(_) -> error.

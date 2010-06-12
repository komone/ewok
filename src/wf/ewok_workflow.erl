%%
-module(ewok_workflow).
% -include("ewok.hrl").

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3, 
	handle_call/3, handle_cast/2, handle_info/2]).

-export([info/0, create/1, route/2, 
	launch/1, next/1, dispatch/1]).

-record(state, {routes = [], process = [], queues = []}).
-record(work_item, {qid, id, attrs = [], payload}).
% -record(work_step, {id, queue, ops = [], lock = []}).

-define(SERVER, ?MODULE).

-define(TIMEOUT, 3000).

-compile(export_all).
	
test(Count) ->
	start_link([{launch, test}, {test, test1}, {test1, test2}, {test2, test3}, {test3, terminate}]),
	test_launch(Count),
	test_process([test, test1, test2, test3]),
	stop().

test_launch(Count) ->
	Time = ewok_util:ftime({?MODULE, launch, [<<"payload">>]}, Count),
	io:format("Launch [~p]: ~pms~n~p~n", [Count, Time, info()]). 

test_process([Qname|T]) ->
	Time = ewok_util:ftime({?MODULE, test_next, [Qname]}),
	{_, Queues} = info(),
	io:format("Process [~p]: ~pms~n~p~n", [Qname, Time, Queues]),
	test_process(T);
test_process([]) ->
	ok.

test_next(Qname) when is_atom(Qname) ->
	case next(Qname) of
	W = #work_item{} ->
		dispatch(W),
		test_next(Qname);
	[] ->
		ok
	end.
%%
start_link(Map) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Map, []).
%%	
stop() ->
	gen_server:cast(?SERVER, stop).
%%
create(Qname) ->
	gen_server:call(?SERVER, {create, Qname}).
%%
route(From, To) ->
	gen_server:call(?SERVER, {route, From, To}).
%%
next(Qname) ->
	gen_server:call(?SERVER, {next, Qname}, ?TIMEOUT).
%%
launch(Payload) ->
	W = #work_item{
		qid = launch,
		id = ewok_identity:key(),
		payload = Payload
	},
	dispatch(W).
%%
dispatch(#work_item{} = W) ->
	gen_server:call(?SERVER, {dispatch, W}, ?TIMEOUT).

%%
info() ->
	gen_server:call(?SERVER, info).

%%
init(Map) ->
	State = parse_map(Map),
	{ok, State}.
%%
handle_call({create, Qname}, _From, State = #state{queues = Queues}) ->
	case lists:keyfind(Qname, 1, Queues) of
	false ->
		{reply, ok, State#state{queues = [{Qname, queue:new()} | Queues]}};
	_ ->
		{reply, {error, exists}, State}
	end;
%
handle_call({route, From, To}, _From, State = #state{routes = Routes, queues = Queues}) ->
	case {lists:keyfind(From, 1, Queues), lists:keyfind(To, 1, Queues)} of
	{false, _} ->
		{reply,{error, invalid_queue}, State};
	{_, false} ->
		{reply, {error, invalid_queue}, State};
	_ ->
		{reply, ok, State#state{routes = [{From, To}|Routes]}}
	end;
%
handle_call({next, Qname}, {From, _}, State = #state{queues = Queues}) ->
	case lists:keyfind(Qname, 1, Queues) of
	{Qname, Queue} ->
		case queue:out(Queue) of
		{{value, {Id, Payload}}, NewQueue} ->
			WorkItem = #work_item{qid = Qname, id = Id, payload = Payload},
			NewState = State#state{
				process = [{Id, From} | State#state.process],
				queues = lists:keyreplace(Qname, 1, Queues, {Qname, NewQueue})
			},
			{reply, WorkItem, NewState};
		{empty, Queue} ->
			{reply, [], State}
		end;
	_ ->
		{reply, {error, undefined}, State}
	end;
%
handle_call({dispatch, #work_item{qid = launch, id = Id, payload = Payload}}, _From, 
		State = #state{routes = Routes, queues = Queues}) ->
	{launch, Qname} = lists:keyfind(launch, 1, Routes),
	{Qname, Queue} = lists:keyfind(Qname, 1, Queues),
	NewQueue = queue:in({Id, Payload}, Queue),
	{reply, {ok, Qname}, State#state{queues = lists:keyreplace(Qname, 1, Queues, {Qname, NewQueue})}};
%
handle_call({dispatch, #work_item{qid = Qname, id = Id, payload = Payload}}, {From, _}, 
		State = #state{routes = Routes, process = Process, queues = Queues}) ->
	case {lists:keyfind(Id, 1, Process), lists:keyfind(Qname, 1, Routes)} of
	{{Id, From}, {Qname, terminate}} ->
		NewProcess = lists:keydelete(Id, 1, Process),
		{reply, {ok, done}, State#state{process = NewProcess}};
	{{Id, From}, {Qname, Next}} ->
		{Next, Queue} = lists:keyfind(Next, 1, Queues),
		NewQueue = queue:in({Id, Payload}, Queue),
		NewQueues = lists:keyreplace(Next, 1, Queues, {Next, NewQueue}),
		NewProcess = lists:keydelete(Id, 1, Process),
		{reply, {ok, Next}, State#state{queues = NewQueues, process = NewProcess}};
	{{Id, _}, _} ->
		{reply, {error, already_locked}, State};
	{_, _} ->
		{reply, {error, invalid_item}, State}
	end;
%
handle_call(info, _From, State = #state{routes = Routes, queues = Queues}) ->
	Info = [{Qname, queue:len(Queue)} || {Qname, Queue} <- Queues],
	{reply, {Routes, Info}, State};
%
handle_call(_Message, _From, State) ->
	{reply, not_implemented, State}.
%%
handle_cast(stop, State) ->
	{stop, normal, State};
%
handle_cast(_Message, State) ->
	{noreply, State}.
%%
handle_info(_Info, State) ->
	{noreply, State}.
%%
code_change(_OldVsn, State, _Extra) ->
	{ok, State}.
%%
terminate(_Reason, _State) ->
	ok.

%%
parse_map(List) ->
	parse_map(List, [], []).
%%
parse_map([R = {Q1, Q2}|T], Routes, Queues) when Q1 =/= Q2 ->
	parse_map(T, [R|Routes], add_queues([Q1, Q2], Queues));
parse_map([], Routes, Queues) ->
	#state{routes = lists:reverse(Routes), queues = lists:reverse(Queues)}.
%%
add_queues([launch|T], Queues) ->
	add_queues(T, Queues);
add_queues([terminate|T], Queues) ->
	add_queues(T, Queues);
add_queues([Q|T], Queues) ->
	case lists:keymember(Q, 1, Queues) of
	false ->
		add_queues(T, [{Q, queue:new()} | Queues]);
	true ->
		add_queues(T, Queues)
	end;
add_queues([], Queues) ->
	Queues.

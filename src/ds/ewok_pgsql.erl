%%
-module(ewok_pgsql).

%% A default postgres connection pool for the ewok server

-behaviour(ewok_service).
-export([start_link/1, stop/0]).
-export([connect/0, run/1, release/1]).

-behaviour(gen_server).
-export([init/1, code_change/3, terminate/2]). 
-export([handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {id, connections, monitors, waiting, opts}).

-define(SERVER, ?MODULE).
-define(DEFAULT_POOL_SIZE, 10).
-define(DEFAULT_POOL_TIMEOUT, 5000).

%% ewok_service
start_link(Opts) ->
	gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

stop() ->
    gen_server:cast(?SERVER, stop).

%% client interface

%%
connect() ->
	get_connection(?DEFAULT_POOL_TIMEOUT).
	
%%
release(C) when is_pid(C) ->
	return_connection(C).

%%
run(Query) ->
	{ok, C} = connect(),
	{ok, _, Result} = pgsql:squery(C, Query),
	release(C),
	Result.

%%
get_connection(Timeout) ->
	gen_server:cast(?SERVER, {get_connection, self()}),
	receive
	{pgsql_pool, ?SERVER, {connection, C}} ->
		?SERVER ! {pgsql_pool, ?SERVER, {ack, self(), C}},
		{ok, C}
	after
	Timeout ->
		gen_server:cast(?SERVER, {cancel_wait, self()}),
		{error, timeout}
	end.
%%
return_connection(C) ->
	gen_server:call(?SERVER, {return_connection, C}).


%% gen_server

%
init(Opts) ->
	process_flag(trap_exit, true),
	Size = ewok_config:get_value({ewok, postgres, pool, size}, ?DEFAULT_POOL_SIZE),
	
	Connections = connect(Size, [
		{host, "localhost"}, 
		{username, "ewok"}, 
		{password, "password"},
		{database, "Test"}
		| Opts]),
		
	State = #state{
		id = ?MODULE,
		opts = Opts,
		connections = Connections,
		monitors = [],
		waiting = queue:new()
	},
	{ok, State}.
%
handle_call({return_connection, C},  _From, State) ->
    #state{monitors = Monitors} = State,
    case lists:keytake(C, 1, Monitors) of
	{value, {C, M}, Monitors2} ->
		erlang:demonitor(M),
		{reply, ok, return(C, State#state{monitors = Monitors2})};
	false ->
		{reply, ok, State}
    end;
%
handle_call(Request, _From, State) ->
    {stop, {unsupported_call, Request}, State}.
%
handle_cast({get_connection, Pid}, State) ->
    #state{connections = Connections, waiting = Waiting} = State,
    case Connections of
	[C | T] -> 
		{noreply, deliver(Pid, C, State#state{connections = T})};
	[] -> 
		{noreply, State#state{waiting = queue:in(Pid, Waiting)}}
    end;
%
handle_cast({cancel_wait, Pid}, State) ->
    #state{waiting = Waiting} = State,
    Waiting2 = queue:filter(fun(P) -> P =/= Pid end, Waiting),
    {noreply, State#state{waiting = Waiting2}};
%
handle_cast(stop, State) ->
    {stop, normal, State};
%
handle_cast(Request, State) ->
    {stop, {unsupported_cast, Request}, State}.
%
handle_info({'DOWN', M, process, _Pid, _Info}, State) ->
    #state{monitors = Monitors} = State,
    case lists:keytake(M, 2, Monitors) of
	{value, {C, M}, Monitors2} ->
		State2 = return(C, State#state{monitors = Monitors2}),
		{noreply, State2};
	false ->
		{noreply, State}
    end;
%
handle_info({'EXIT', Pid, _Reason}, State) ->
    #state{opts = Opts, connections = Connections, monitors = Monitors} = State,
    Connections2 = lists:delete(Pid, Connections),
    F = fun({C, M}) when C == Pid -> erlang:demonitor(M), false;
           ({_, _})               -> true
        end,
    Monitors2 = lists:filter(F, Monitors),
    [C] = connect(1, Opts),
    State2 = return(C, State#state{connections = Connections2, monitors = Monitors2}),
    {noreply, State2};
%
handle_info({pgsql_pool, P, {ack, Pid, _C}}, #state{id = P} = State) ->
    error_logger:error_msg("pgsql_pool ~p received late ack from ~p~n", [P, Pid]),
    {noreply, State};
%
handle_info(Info, State) ->
    {stop, {unsupported_info, Info}, State}.
%
terminate(_Reason, _State) ->
    ok.
%
code_change(_OldVsn, State, _Extra) ->
    State.

%% internal

%%
connect(N, Opts) ->
    connect(N, Opts, []).
%%
connect(0, _Opts, Acc) ->
    Acc;
%
connect(N, Opts, Acc) ->
    Host = proplists:get_value(host, Opts),
    Username = proplists:get_value(username, Opts),
    Password = proplists:get_value(password, Opts),
    {ok, C} = pgsql:connect(Host, Username, Password, Opts),
    connect(N - 1, Opts, [C | Acc]).
%%
deliver(Pid, C, State) ->
    #state{id = Id, connections = Connections, monitors = Monitors} = State,
    Pid ! {pgsql_pool, Id, {connection, C}},
    receive
	{pgsql_pool, Id, {ack, Pid, C}} ->
		M = erlang:monitor(process, Pid),
		Monitors2 = [{C, M} | Monitors],
		State#state{monitors = Monitors2}
    after
	100 ->
		State#state{connections = [C | Connections]}
    end.
%%
return(C, State = #state{connections = Connections, waiting = Waiting}) ->
    case queue:out(Waiting) of
	{{value, Pid}, Waiting2} ->
		State2 = deliver(Pid, C, State),
		State2#state{waiting = Waiting2};
	{empty, _Waiting} ->
		Connections2 = [C | Connections],
		State#state{connections = Connections2}
    end.

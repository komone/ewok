%%% Author gen_cluster: nmurray@attinteractive.com, alerner@attinteractive.com
-module(ewok_cluster).
-include("ewok.hrl").
%% gen_cluster behaviour
-export([behaviour_info/1]).

%%
-behaviour(gen_server).
-export([init/1, terminate/2, code_change/3,
	handle_call/3, handle_cast/2, handle_info/2]).
%% Export the same API as gen_server.
-export([start/3, start/4, start_link/3, start_link/4,
    call/2, call/3, cast/2, reply/2, abcast/2, abcast/3,
    multi_call/2, multi_call/3, multi_call/4,
    enter_loop/3, enter_loop/4, enter_loop/5, wake_hib/5]).

%% Helper functions
-export([plist/1]).

%% State data record.
-record(state, {module, state, data, plist, seed}).

%%
behaviour_info(callbacks) -> [
	% gen_cluster 
	{handle_join, 3}, {handle_node_joined, 3}, {handle_leave, 4},
    % gen_server      
	{init,1}, {handle_call,3},{handle_cast,2},{handle_info,2}, {terminate,2},{code_change,3}
];

behaviour_info(_) ->
    undefined.

%% Users will use these start functions instead of gen_server's.
%% We add the user's module name to the arguments and call
%% server's start function with our module name instead.
start(Mod, Args, Options) ->
    gen_server:start(?MODULE, [Mod, Args], Options).
start(Name, Mod, Args, Options) ->
    gen_server:start(Name, ?MODULE, [Mod, Args], Options).
start_link(Mod, Args, Options) ->
    gen_server:start(?MODULE, [Mod, Args], Options).
start_link(Name, Mod, Args, Options) ->
    gen_server:start(Name, ?MODULE, [Mod, Args], Options).

%% Delegate the rest of the reqests to gen_server
call(Name, Request) ->
    gen_server:call(Name, Request).
call(Name, Request, Timeout) ->
    gen_server:call(Name, Request, Timeout).
cast(Name, Request) ->
    gen_server:cast(Name, Request).
reply(To, Reply) ->
    gen_server:reply(To, Reply).
abcast(Name, Request) ->
    gen_server:abcast(Name, Request).
abcast(Nodes, Name, Request) ->
    gen_server:abcast(Nodes, Name, Request).
multi_call(Name, Req) ->
    gen_server:multi_call(Name, Req).
multi_call(Nodes, Name, Req)  ->
    gen_server:multi_call(Nodes, Name, Req).
multi_call(Nodes, Name, Req, Timeout)  ->
    gen_server:multi_call(Nodes, Name, Req, Timeout).
enter_loop(Mod, Options, State) ->
    gen_server:enter_loop(Mod, Options, State).
enter_loop(Mod, Options, State, Timeout) ->
    gen_server:enter_loop(Mod, Options, State, Timeout).
enter_loop(Mod, Options, State, ServerName, Timeout) ->
    gen_server:enter_loop(Mod, Options, State, ServerName, Timeout).
wake_hib(Parent, Name, State, Mod, Debug) ->
    gen_server:wake_hib(Parent, Name, State, Mod, Debug).

%%
plist(PidRef) -> % {ok, Plist}
    call(PidRef, {'$gen_cluster', plist}).


%% run the user's init/1 and store the user's state data in our internal
%% state data record for later reference.
init([Mod, Args]) ->
    Seed = 
		case Args of
        {seed, Value} -> 
			Value;
        _ -> 
			undefined
		end,
    InitialState = #state{module = Mod, plist = [self()], seed = Seed},
    {ok, State1} = join_existing_cluster(InitialState),
    {_Resp, State2} = start_cluster_if_needed(State1),
 
    case Mod:init(Args) of
	{ok, ExtState} ->
		StateData = State2#state{module = Mod, state = ExtState},
		{ok, StateData};
	{ok, ExtStateName, ExtStateData} -> 
		StateData = State2#state{module = Mod, state = ExtStateName, data = ExtStateData},
		{ok, StateData};
	{ok, ExtStateName, ExtStateData, Timeout} ->
		StateData = State2#state{module = Mod, state = ExtStateName, data = ExtStateData},
		{ok, StateData, Timeout};
	{stop, Reason} ->
		{stop, Reason};
	Other ->
	  ?TTY({init, Other}),
	  exit(bad_gen_cluster_init_call) % hmmm
    end.

%%
handle_call({'$gen_cluster', join}, From, State) ->
    ?TTY({'$gen_cluster', join, State}),
    {ok, NewState} = handle_node_joining(From, State),
    Reply = {ok, NewState#state.plist},
    {reply, Reply, NewState};
%%
handle_call({'$gen_cluster', joined_announcement, KnownRing}, From, State) ->
    ?TTY({'$gen_cluster', joined_announcement, State}),
    {ok, NewState} = handle_node_joined_announcement(From, KnownRing, State),
    Reply = {ok, NewState#state.plist},
    {reply, Reply, NewState};
%%
handle_call({'$gen_cluster', plist}, _From, State) ->
    Reply = {ok, State#state.plist},
    {reply, Reply, State};
%%
handle_call(Request, From, State) -> 
	Mod = State#state.module,
	ExtState = State#state.state,
	Reply = Mod:handle_call(Request, From, ExtState),
	handle_call_reply(Reply, From, State).

% handle the replies by updating and substituting our own state
handle_call_reply({reply, Reply, ExtState}, _From, State) ->
	NewState = State#state{state=ExtState},
	{reply, Reply, NewState};
%%
handle_call_reply({reply, Reply, ExtState, Timeout}, _From, State) ->
	NewState = State#state{state=ExtState},
	{reply, Reply, NewState, Timeout};
%%
handle_call_reply({noreply, ExtState}, _From, State) ->
	NewState = State#state{state=ExtState},
	{noreply, NewState};
%%
handle_call_reply({noreply, ExtState, Timeout}, _From, State) ->
	NewState = State#state{state=ExtState},
	{noreply, NewState, Timeout};
%%
handle_call_reply({stop, Reason, Reply, ExtState}, _From, State)  ->
	NewState = State#state{state=ExtState},
	{stop, Reason, Reply, NewState};
%%
handle_call_reply({stop, Reason, ExtState}, _From, State) ->
	NewState = State#state{state=ExtState},
	{stop, Reason, NewState}.
% handle Other?

%%
handle_cast(Msg, State) -> 
    Mod = State#state.module,
    ExtState = State#state.state,
    Reply = Mod:handle_cast(Msg, ExtState),
    handle_cast_info_reply(Reply, State).
%%
handle_info({'DOWN', MonitorRef, process, Pid, Info}, State) ->
    ?TTY({'DOWN', removing_node, Pid, Info}),
    ExtState = State#state.state,
    Mod = State#state.module,

    case does_pid_exist_in_plist(Pid, State) of
	true ->
		{ok, NewState2} = remove_pid_from_plist(Pid, State),
		Pidlist = NewState2#state.plist,
		{ok, NewExtState} = Mod:handle_leave(Pid, Pidlist, Info, ExtState),
		NewState3 = NewState2#state{state=NewExtState},
		{noreply, NewState3};
	false ->
		Reply = Mod:handle_info({'DOWN', MonitorRef, process, Pid, Info}, ExtState),
		handle_cast_info_reply(Reply, State)
    end;
%%
handle_info(Info, State) -> 
	Mod = State#state.module,
	ExtState = State#state.state,
	Reply = Mod:handle_info(Info, ExtState),
	handle_cast_info_reply(Reply, State).
%%
handle_cast_info_reply({noreply, ExtState}, State) ->
	NewState = State#state{state=ExtState},
	{noreply, NewState};
%%
handle_cast_info_reply({noreply, ExtState, Timeout}, State) ->
	NewState = State#state{state=ExtState},
	{noreply, NewState, Timeout};
%%
handle_cast_info_reply({stop, Reason, ExtState}, State) ->
	NewState = State#state{state=ExtState},
	{stop, Reason, NewState}.
%%
terminate(Reason, State) -> 
    Mod = State#state.module,
    ExtState = State#state.state,
    _ = Mod:terminate(Reason, ExtState),
    ok.
%%
code_change(OldVsn, State, Extra) -> 
    Mod = State#state.module,
    ExtState = State#state.state,
    {ok, NewExtState} = Mod:code_change(OldVsn, ExtState, Extra),
    NewState = State#state{state = NewExtState},
    {ok, NewState}.

%%--------------------------------------------------------------------
%% Func: handle_node_joining(OtherNode, State) -> {ok, NewState}
%% Description: Called when another node joins the server cluster. 
%%--------------------------------------------------------------------
handle_node_joining({OtherPid, _Tag}, State) ->
	{ok, NewState} = add_pid_to_plist(OtherPid, State),
	% callback
	#state{module = Mod, plist = Plist, state = ExtState} = NewState,
	{ok, NewExtState} = Mod:handle_join(OtherPid, Plist, ExtState),
	% update the external state
	StateData = NewState#state{state = NewExtState},
	{ok, StateData}.

%%--------------------------------------------------------------------
%% Func: handle_node_joined_announcement(KnownRing, State) -> {ok, NewState}
%% Description: When a node joins a known server, it then broadcasts to all
%% other servers that it joined. It tells all other servers about the entire
%% pidlist it received from the known node. This is a check to make sure that
%% everytime a node joins all the other nodes know about it as well as every
%% other node in the cluster.
%% 
%% TODO, consider removing this method entirely
%%--------------------------------------------------------------------
handle_node_joined_announcement({OtherPid, _Tag}, KnownRing, State) ->
    {ok, NewState} = add_pids_to_plist(KnownRing, State),

    % callback
    #state{module = Mod, plist = Plist, state = ExtState} = NewState,
    {ok, NewExtState} = Mod:handle_node_joined(OtherPid, Plist, ExtState),

    % update the external state
    StateData = NewState#state{state = NewExtState},

    {ok, StateData}.

%% Func: join_existing_cluster(State) -> {ok, NewState} | false
%% Description: Look for any existing servers in the cluster, try to join them
join_existing_cluster(State) ->
	Servers = get_seed_nodes(State),
	connect_to_servers(Servers),
	global:sync(), % otherwise we may not see the pid yet
	NewState = 
		case whereis_global(State) of % join unless we are the main server 
		undefined ->
			?TTY({join_existing_cluster, undefined}),
			State;
		X when X =:= self() ->
			?TTY({join_existing_cluster, self, skip, X}),
			State;
		_ ->
			?TTY({join_existing_cluster, whereis_global(State)}),
			?TTY({join_existing_cluster, State, State#state.module}),
			{ok, KnownPlist} = gen_cluster:call({global, globally_registered_name(State)}, {'$gen_cluster', join}),
			{ok, NewInformedState} = add_pids_to_plist(KnownPlist, State),
			broadcast_join_announcement(NewInformedState)
		end,
	{ok, NewState}.
%%
connect_to_servers(ServerNames) ->
	?TTY({connect_to_servers, ServerNames}),
	F = fun(Server) ->
			case Server of
			undefined -> 
				?TTY({"warning, skipping server", Server}),
				skip; % do nothing
			_ ->
				?TTY({"connecting to server: ", Server}),
				Node = Server,
				case net_adm:ping(Node) of
				pong ->
					ok;
				_ ->
					?TTY({"WARNING: ping of Node failed:", Node}) 
					% should this be a bigger failure? how should we handle this so that way 
					% the first server doesn't always have to have this problem?
				end
			end
		end,
	ServerRefs = lists:map(F, ServerNames),
	{ok, ServerRefs}.

%%--------------------------------------------------------------------
%% Func: start_cluster_if_needed(State) -> {{ok, yes}, NewState} |
%%                                         {{ok, no}, NewState}
%% Description: Start cluster if we need to
%%--------------------------------------------------------------------
start_cluster_if_needed(State) ->
	global:sync(), % otherwise we may not see the pid yet
	{Resp, NewState} = 
		case whereis_global(State) of
		undefined ->
			start_cluster(State);
		_ ->
			{no, State}
		end,
	{{ok, Resp}, NewState}.

whereis_global(State) ->
	global:whereis_name(globally_registered_name(State)).

%% gen_cluster will globally register a pid of the format below. This allows
%% for each module that becomes a gen_cluster to have a central rally point and
%% will not confluct with other modules using gen_cluster
globally_registered_name(State) ->
	Mod = State#state.module,
	"gen_cluster_" ++ atom_to_list(Mod).

%%--------------------------------------------------------------------
%% Func: start_cluster(State) -> {yes, NewState} | {no, NewState}
%% Description: Start a new cluster, basically just globally register a pid for joining
%%--------------------------------------------------------------------
start_cluster(State) ->
	?TTY({"Starting server:", globally_registered_name(State)}),
	RegisterResp = global:register_name(globally_registered_name(State), self()),
	{RegisterResp, State}.

add_pids_to_plist([Head|OtherPids], State) ->
	{ok, NewState} = add_pid_to_plist(Head, State),
	add_pids_to_plist(OtherPids, NewState);
add_pids_to_plist([], State) ->
	{ok, State}.

add_pid_to_plist(OtherPid, State) ->
	% Exists = lists:any(fun(Elem) -> Elem =:= OtherPid end, State#state.plist),
	NewPlist = 
		case does_pid_exist_in_plist(OtherPid, State) of
		true ->
			State#state.plist;
		false ->
			% monitor that pid
			erlang:monitor(process, OtherPid),
			% add the other pid to our plist
			[OtherPid|State#state.plist]
		end,
	NewState  = State#state{plist=NewPlist},
	{ok, NewState}.

does_pid_exist_in_plist(OtherPid, State) -> % bool() 
	lists:any(fun(Elem) -> Elem =:= OtherPid end, State#state.plist).

remove_pid_from_plist(OtherPid, State) ->
	NewPlist = lists:delete(OtherPid, State#state.plist),
	NewState = State#state{plist=NewPlist},
	{ok, NewState}.

broadcast_join_announcement(State) ->
	NotSelfPids = lists:delete(self(), State#state.plist),
	NotGlobalPids = lists:delete(whereis_global(State), NotSelfPids),
	[call(Pid, {'$gen_cluster', joined_announcement, State#state.plist}) || Pid <- NotGlobalPids],
	State.

% list of Nodes
% Node will be sent to net_adm:ping
get_seed_nodes(State) ->
    case init:get_argument(gen_cluster_known) of
	{ok, [[Server]]} ->
		[list_to_atom(Server)];
	_ ->
		case State#state.seed of
		[Server|_Servers] ->
			?TTY({"got seed servers", foo}),
			[{Server, undefined}];
		_ ->
			[undefined]
		end
	end.

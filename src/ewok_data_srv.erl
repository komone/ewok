%%
%%
-module(ewok_data_srv).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("../include/ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_service).
-export([start_link/0, stop/0, service_info/0]).
-export([info/0, connect/1]).

-behaviour(gen_server).
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-record(state, {ds=[]}).

-define(SERVER, ?MODULE).

%%
service_info() -> [
	{name, "Ewok Data Service"},
	{version, {1,0,0}},
	{comment, ""}
].

%% This indirection expects to allow for multiple DS
start_link() ->
	ewok_log:log(default, service, {?MODULE, service_info()}),
	
	ewok:configure(),
	
	DefaultDS = ewok_config:get({ewok, datasource, default}, mnesia),
	Module = ewok_config:get({ewok, datasource, DefaultDS, mod}, ewok_mnesia_ds),
	DataDir = ewok_config:get({ewok, datasource, data_path}, "./priv/data"),
	%% NOTE! must be an atom
	DSDataDir = filename:join([ewok_util:appdir(), DataDir, atom_to_list(DefaultDS)]),
	
	DS = Module:new(),
	case DS:init([{dir, DSDataDir}]) of
	Spec = #datasource{} ->
		gen_server:start_link({local, ?SERVER}, ?MODULE, 
			[Spec#datasource{name="Ewok DefaultDS", id=default}], []);
	Error -> 
		Error
	end.

%%
stop() ->
    gen_server:cast(?SERVER, stop),
	mnesia:stop().	

%%
info() ->
	gen_server:call(?SERVER, {info}, infinity).

%%
connect(default) ->
	DefaultDS = ewok_config:get({ewok, datasource, default}, mnesia),
	connect(DefaultDS);
connect(mnesia) ->
	{ok, ewok_mnesia_ds:new()};
%%
connect({postgres}) ->
	not_implemented;
%%
connect({aws, sdb}) ->
    AWS_ACCESS_KEY = ewok_config:get("ewok.aws.sdb_access_key"),
	AWS_SECRET_KEY = ewok_config:get("ewok.aws.sdb_secret_key"),
	case AWS_ACCESS_KEY =/= undefined andalso AWS_SECRET_KEY =/= undefined of
	true ->	{ok, erlaws_sdb:new(AWS_ACCESS_KEY, AWS_SECRET_KEY, true)};
	false -> {error, no_access}
	end.
	
%%
%%% gen_server
%%
init(Args) ->
    process_flag(trap_exit, true), % why do we need this?
    {ok, #state{ds=Args}}.
%
handle_call({info}, _From, State) ->
	{reply, State#state.ds, State};
%
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
terminate(_Reason, _State) ->
    ok.
%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.

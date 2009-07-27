%% A mechanism to cache templates...
%% TODO: not yet implemented!
-module(ewok_template_srv).
-vsn("1.0").
-author('steve@simulacity.com').

-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

%% API
-export([start_link/0, stop/0]).
-export([add_dir/1, remove_dir/1, refresh/0, get_template/1]).
-export([dump/0]).

-define(SERVER, ?MODULE).
-define(ETS, ?MODULE).

%%% 
start_link() ->
	start_link([]).
start_link(DirList) when is_list(DirList) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, DirList, []).
stop() ->
    gen_server:cast(?SERVER, stop).

%%
add_dir(Path) ->
	gen_server:call(?SERVER, {add_dir, Path}, infinity).

%%
remove_dir(Path) ->
    case ets:lookup(?ETS, Path) of
	[_S] -> 
		ets:delete(?ETS, Path);
	[] ->
		ok
    end.

%%
refresh() ->
	gen_server:call(?SERVER, {refresh}, infinity).

%%
get_template(Path) ->
    case ets:lookup(?ETS, Path) of
	[T] ->  T;
	_ -> undefined
	end.

	
dump() ->
    Templates = ets:tab2list(?ETS),
    io:format("** ~p available templates...~n", [length(Templates)]),
    lists:foreach(fun(S) -> io:format("~p~n", [S]) end, Templates).

%%
%%% Callback functions from gen_server
%%
init(DirList) ->
	[X || X <- DirList], %% CHECK AND LOAD TEMPLATES
    ets:new(?ETS, [set, named_table, protected]),
    {ok, undefined, []}.

%%
handle_call({add_dir, Path}, _From, State) ->
	Template = [], % IMPLEMENT!
    ets:insert_new(?ETS, {Path, Template}),
    {reply, {Path, Template}, State}.

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

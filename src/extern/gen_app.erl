%%%------------------------------------------------------------------------ - 
%%% Author: Kiran Khaladkar 
%%% Description: Behavior for writing web applications. 
%%% Created: Sat 5th Dec 2009 
%%%------------------------------------------------------------------------ - 
-module(gen_app). 
-export([behavior_info/1, start/3, start/2]). 

-behavior(gen_server). 

-record(gen_app_state, {
	app_name :: atom(),         %%stores app name 
	cookie :: string(),         %%cookie to be stored to refer to client 
	app_state :: tuple()        %%state maintained by the apps extending this behavior 
}).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, 
         terminate/2, code_change/3]).
		 
-export([get_info/1, get_cookie/1, set_cookie/2, 
	handle_sync_request/2, handle_async_request/2]). 

behavior_info(callbacks) -> 
    [{init,1}, {handle_request, 2}, {respond, 1}, {terminate,1}]. 

%% Function: start/3 
%% Description: starts the application by AppName with Params and Options. 
%%        Options are gen_app process options 
%% Return: {ok, Pid} 
start(AppName, Param, Options) -> 
    gen_server:start_link(?MODULE, {AppName,Param}, Options). 

%% Function: start/2 
%% Description: Starts the application by AppName with params but without Options 
%% Return: {ok, Pid} 
start(AppName, Param) -> 
    gen_server:start_link(?MODULE, {AppName, Param}, []). 

init({AppName, Param}) -> 
    {ok, AppInitCookie, AppInitState} = AppName:init(Param), 
    {ok, #gen_app_state{app_name = AppName, cookie = AppInitCookie, app_state = AppInitState}}. 
	
%% Function: get_info/1 
%% Return: returns state of the gen_app 
get_info(AppId) -> 
    gen_app:handle_sync_request(AppId, get_state). 
	
get_cookie(AppId) -> 
    handle_sync_request(AppId, get_cookie). 

set_cookie(AppId, Cookie) -> 
    handle_sync_request(AppId, {set_cookie, Cookie}). 
	
%% Function: handle_async_request(/2 
%% Description: API to handle asynchronous calls that return immidiately without results. 
%% Rerurn: {noreply, State} 
handle_async_request(AppId, Reqest) -> 
    gen_server:handle_cast(AppId, {app_req, Reqest}), 
    {ok, processing}. 
	
%% Function: handle_sync_request/2 
%% Description: API to handle synchronous calls that return the result of the 
%%        request after completion of the processing of that request 
%% Return: {reply, Reply, NewState} after completion of the processing 
handle_sync_request(AppId, Request) -> 
    gen_server:handle_call(AppId, {app_req, Request}). 
	
%% Function: handle_cast/2 
%% Desciption: handle cast messages to gen_app process 
%% Return: {noreply, #state{}} 
handle_cast({app_req, Request}, #gen_app_state{app_name=AppName, app_state=AppState} = State) -> 
    {ok, NewAppState, Response} = AppName:handle_request(Request, AppState), 
    AppName:respond(Response), 
    {noreply, State#gen_app_state{app_state=NewAppState}}; 
handle_cast(_Msg, _State) -> 
	{noreply, _State}. 

%% Function: handle_call/3 
%% Desciption: handle cast messages to gen_app process 
%% Return: {noreply, #state{}} 
handle_call(get_state,_From, State) -> 
    {reply, app_state, State}; 
	
handle_call({app_req, Req},_From, #gen_app_state{app_name = AppName, app_state = AppState} = State) -> 
    {ok, NewAppState, Response} = AppName:handle_request(Req, AppState), 
    AppName:respond(Response), 
    {reply, ok, State#gen_app_state{app_state=NewAppState}}; 

handle_call(get_cookie, _From, State) -> 
    {reply, State#gen_app_state.cookie, State}; 

handle_call({set_cookie, Cookie}, _From, State)-> 
    {reply, ok, State#gen_app_state{cookie = Cookie}}; 

handle_call(_Msg,_From, _State) -> 
    Reply = _Msg, 
    {reply, Reply, _State}. 

handle_info(_Info, _State) -> 
    {noreply, _State}. 

code_change(_OldVsn, State, _Extra) -> 
    {ok, State}. 

terminate(Reason, #gen_app_state{app_name=AppName, app_state=AppState} = _State) -> 
    AppName:terminate(Reason, AppState). 

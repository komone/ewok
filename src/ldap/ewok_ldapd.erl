-module(ewok_ldapd).
-include("ewok.hrl").
-include("ewok_system.hrl").
-include("ldap.hrl").

-compile(export_all).

-define(SP, <<" ">>).
-define(CRLF, <<"\r\n">>).

-define(SOCKET_TIMEOUT_SECONDS, 600).

-behaviour(ewok_inet).
-export([start/1, init/2, terminate/3]).

% Temp
-export([start/0]).

%
-record(state, {id, ip, from, to=[], body=[]}).

start() ->
	ewok_logging_srv:start_link([]),
	ewok_db:start(),
	ewok_identity_srv:start_link([]),
	Inet = start(?LDAP_PORT),
	ewok_inet:start_link(Inet).
	
start(Port) ->
	#ewok_inet{
		port = Port,
		protocol = ldap,
		handler = ?MODULE,
		codec = ewok_ldap
	}.
	
%% callbacks: ewok_inet
init(_Options, RemoteIp) ->
	{noreply, bind, #state{ip = RemoteIp}}.
%%
terminate(_Reason, _StateName, _State) ->
    ok.

%%
%% state callbacks
%%
bind(_R = #bind_request{}, State) ->
	{reply, service_ready, session, State};
bind(timeout, State) ->
	{reply, protocol_error, terminate, State};
bind(_Message, State) ->
	{reply, protocol_error, terminate, State}. 

%%
session(_R = #search_request{}, State) ->
	{reply, success, session, State};
session(#unbind_request{}, State) ->
	{reply, success, bind, State};
session(_Message, State) ->
	{reply, unwilling_to_perform, session, State}.



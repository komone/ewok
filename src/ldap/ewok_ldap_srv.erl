-module(ewok_ldap_srv).
-include("ewok.hrl").
-include("ewok_system.hrl").
-include("ldap.hrl").

-compile(export_all).

-define(SP, <<" ">>).
-define(CRLF, <<"\r\n">>).

-define(SOCKET_TIMEOUT_SECONDS, 600).

-behaviour(ewok_service).
-export([start_link/1, stop/0]).
-export([service/2]).

start_link(_Args) -> 
	try begin
		ewok_log:info({service, ?MODULE}),
		%% DS = ewok_data_srv:connect(default)
		Transport = gen_tcp, %%
		Port = ewok_config:get_value({ewok, ldap, port}, ?LDAP_PORT),
		SocketOpts = ewok_socket:configure(Transport, {ewok, ldap}),
		MaxConnections = ewok_config:get_value({ewok, ldap, max_connections}, infinity),
		Timeout = ewok_config:get_value({ewok, ldap, timeout}, ?SOCKET_TIMEOUT_SECONDS) * 1000,
		
		Handler = fun(X) -> ?MODULE:service(X, Timeout) end,
		
		Configuration = [
			{name, ?MODULE},
			{transport, Transport}, %% defines use of gen_tcp or ssl module
			{port, Port},
			{protocol, ldap},
			{max_connections, MaxConnections},
			{socket_opts, SocketOpts},
			{handler, Handler}
		],
		%% ?TTY("CONFIG:~n~p~n", [Configuration]),
		ewok_log:message(configuration, {?MODULE, Configuration}),
		%% Starts a TCP Server for UTP
		{ok, Pid} = ewok_socket_srv:start_link(?MODULE, Configuration),
		ewok_log:add_log(ldap),
		{ok, Pid}
	end catch
	Error:Reason -> 
		{Error, Reason}
	end.

stop() -> 
	ewok_socket_srv:stop(?MODULE),
	ewok_log:remove_log(ldap).

%% Callback from ewok_socket_srv, handing over a client connection
service(Socket, Timeout) ->
	{ok, Session} = ewok_ldap_session:create(),
	ewok_log:message(ldap, {session, [{pid, Session}, {peer, ewok_socket:peername(Socket)}]}),
	?TTY({session, Session}),
	service(Socket, Session, 1, Timeout).
	
%%
service(Socket, Session, Sequence, Timeout) ->
	try begin
		{ok, Packet} = ewok_socket:recv(Socket, 0, Timeout),
		Request = ldap:decode(Packet),
		?TTY({request, Request}),
		Response = ewok_ldap_session:request(Session, Request),
		?TTY({response, Response}),
		Data = ldap:encode(Response),
		ok = ewok_socket:send(Socket, Data),
		service(Socket, Session, Sequence + 1, Timeout)
	end catch
	Error:Reason ->
		ewok_ldap_session:close(Session),
		?TTY({Error, Reason}),
		exit(normal)		
	end.

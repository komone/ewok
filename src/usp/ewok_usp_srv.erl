%%
-module(ewok_usp_srv).

-compile(export_all).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("usp.hrl").

%% Universal Service Protocol
%% Suggested IANA Port 30
%% Suggested Well-known ports 3300-3301
%% Stateless?

-behaviour(ewok_service).
-export([start_link/1, stop/0]).
-export([service/2]).

start_link(_Args) -> 
	try begin
		ewok_log:message(service, ?MODULE),
		%% DS = ewok_data_srv:connect(default)
		Transport = gen_tcp, %%
		Port = ewok:config({ewok, usp, port}, 30),
		SocketOpts = ewok_socket:configure(Transport, {ewok, usp}),
		MaxConnections = ewok:config({ewok, usp, max_connections}, infinity),
		Timeout = ewok:config({ewok, usp, timeout}, 10) * 1000,
		
		Handler = fun(X) -> ?MODULE:service(X, Timeout) end,
		
		Configuration = [
			{name, ?MODULE},
			{transport, Transport}, %% defines use of gen_tcp or ssl module
			{port, Port},
			{protocol, umtp},
			{max_connections, MaxConnections},
			{socket_opts, SocketOpts},
			{handler, Handler}
		],
		%% ?TTY("CONFIG:~n~p~n", [Configuration]),
		ewok_log:message(configuration, {?MODULE, Configuration}),
		%% Starts a TCP Server for UTP
		{ok, Pid} = ewok_socket_srv:start_link(?MODULE, Configuration),
		ewok_log:add_log(mail),
		{ok, Pid}
	end catch
	Error:Reason -> {Error, Reason}
	end.

stop() -> 
	ewok_socket_srv:stop(?MODULE),
	ewok_log:remove_log(mail).


%% Callback from ewok_socket_srv, handing over a client connection
service(Socket, Timeout) ->
	case ewok_socket:recv(Socket, 0, Timeout) of
	{ok, Packet} ->
		try begin
			Message = binary_to_term(Packet),
			?TTY({usp, message, Message}),
			reply(Socket, {reply, ok})
		end catch 
		Error:Reason ->
			reply(Socket, {Error, Reason})
		end;
	{error, Reason} ->
		?TTY({error, Reason})
	end.
	
reply(Socket, Term) ->
	Data = term_to_binary(Term),
	case ewok_socket:send(Socket, Data) of
	ok -> 
		ok;
	_ -> 
		exit(normal)
	end.
	

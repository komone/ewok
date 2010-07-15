-module(ewok_smtpc).

-compile(export_all).

%% test
sendmail(From, To, Message) ->
    {ok, Host} = inet:gethostname(), 
	{ok, Socket} = gen_tcp:connect(Host, 2525, [{active, false}]),
	print(Socket),
	gen_tcp:send(Socket, "EHLO simulacity.com\r\n"),
	print(Socket),
	gen_tcp:send(Socket, "MAIL FROM:" ++ From ++ "\r\n"),
	print(Socket),
	gen_tcp:send(Socket, "RCPT TO:" ++ To ++ "\r\n"),
	print(Socket),
	gen_tcp:send(Socket, "DATA\r\n"),
	print(Socket),	
	gen_tcp:send(Socket, Message ++ "\r\n.\r\n"),
	print(Socket),
	gen_tcp:send(Socket, "QUIT\r\n"),
	print(Socket),
	gen_tcp:close(Socket).


print(Socket) ->
	case gen_tcp:recv(Socket, 0, 10000) of
	{ok, Packet} -> 
		io:format(Packet);
	Error ->
		io:format("ERROR: ~p~n", [Error])
	end.

connect() ->
	ewok_smtp_srv:start_link([]).

send(Command) ->
	gen_fsm:sync_send_event(ewok_smtpd_session, {Command}).
send(Command, Args) ->
	gen_fsm:sync_send_event(ewok_smtpd_fsm, {Command, Args}).

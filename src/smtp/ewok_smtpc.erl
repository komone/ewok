-module(ewok_smtpc).

-compile(export_all).

connect() ->
	ewok_smtpd_fsm:start_link([]).

send(Command) ->
	gen_fsm:sync_send_event(ewok_smtpd_fsm, {Command}).
send(Command, Args) ->
	gen_fsm:sync_send_event(ewok_smtpd_fsm, {Command, Args}).
	

-module(ewok_imapc).
-include("ewok.hrl").

-compile(export_all).

test() ->
    {ok, RawSocket} = ewok_socket:connect(gen_tcp, <<"localhost">>, 143, [binary, {active, false}, {packet, 0}]),
	Socket = {gen_tcp, RawSocket},
	?TTY(read(Socket)),
	?TTY(request(Socket, <<"a001 login admin password\r\n">>)),
	?TTY(request(Socket, <<"a002 capability\r\n">>)),
	?TTY(request(Socket, <<"a003 logout\r\n">>)),
	ewok_socket:close(Socket).

read(Socket) ->
	{ok, Bin} = ewok_socket:recv(Socket, 0, 10000),
	Bin.

request(Socket, Data) ->	
	ok = ewok_socket:send(Socket, Data),
	{ok, Bin} = ewok_socket:recv(Socket, 0, 10000),
	Bin.
	
%S:   * OK IMAP4rev1 Service Ready
%C:   a001 login mrc secret
%S:   a001 OK LOGIN completed
%C:   a002 select inbox
%S:   * 18 EXISTS
%S:   * FLAGS (\Answered \Flagged \Deleted \Seen \Draft)
%S:   * 2 RECENT
%S:   * OK [UNSEEN 17] Message 17 is the first unseen message
%S:   * OK [UIDVALIDITY 3857529045] UIDs valid
%S:   a002 OK [READ-WRITE] SELECT completed
%C:   a003 fetch 12 full
%S:   * 12 FETCH (FLAGS (\Seen) INTERNALDATE "17-Jul-1996 02:44:25 -0700"
%      RFC822.SIZE 4286 ENVELOPE ("Wed, 17 Jul 1996 02:23:25 -0700 (PDT)"
%      "IMAP4rev1 WG mtg summary and minutes"
%      (("Terry Gray" NIL "gray" "cac.washington.edu"))
%      (("Terry Gray" NIL "gray" "cac.washington.edu"))
%      (("Terry Gray" NIL "gray" "cac.washington.edu"))
%      ((NIL NIL "imap" "cac.washington.edu"))
%      ((NIL NIL "minutes" "CNRI.Reston.VA.US")
%      ("John Klensin" NIL "KLENSIN" "INFOODS.MIT.EDU")) NIL NIL
%      "<B27397-0100000@cac.washington.edu>")
%       BODY ("TEXT" "PLAIN" ("CHARSET" "US-ASCII") NIL NIL "7BIT" 3028 92))
%S:    a003 OK FETCH completed
%C:    a004 fetch 12 body[header]
%S:    * 12 FETCH (BODY[HEADER] {350}
%S:    Date: Wed, 17 Jul 1996 02:23:25 -0700 (PDT)
%S:    From: Terry Gray <gray@cac.washington.edu>
%S:    Subject: IMAP4rev1 WG mtg summary and minutes
%S:    To: imap@cac.washington.edu
%S:    cc: minutes@CNRI.Reston.VA.US, John Klensin <KLENSIN@INFOODS.MIT.EDU>
%S:    Message-Id: <B27397-0100000@cac.washington.edu>
%S:    MIME-Version: 1.0
%S:    Content-Type: TEXT/PLAIN; CHARSET=US-ASCII
%S:
%S:    )
%S:    a004 OK FETCH completed
%C:    a005 store 12 +flags \deleted
%S:    * 12 FETCH (FLAGS (\Seen \Deleted))
%S:    a005 OK +FLAGS completed
%C:    a006 logout
%S:    * BYE IMAP4rev1 server terminating connection
%S:    a006 OK LOGOUT completed

%%
%% File: email.hrl
%% Version: 1.0.0 beta
%% Author: Steve Davis <steve@simulacity.com>
%% Updated: October 30, 2009
%%
-define(CRLF, <<$\r,$\n>>).
-define(SP, <<$ >>).

-record(mailbox, {user_id, messages=[]}).
-record(message, {id, timestamp, flags, body}).

%% erlmail versions - PENDING DELETION
-record(store, {name = [], options = []}).
-record(domain, {name = [], options = [] }).

-record(message2, {
	name         = [], % Tuple {Message Name, User Name, Doamin Name}
	from         = [], % single address for sender
	to           = [], % address list for recepient
	cc           = [], % address list for carbon copy
	bcc          = [], % address list for blind carbon copy
	internaldate = [], % date message was received
	size         = 0,  % integer() size of message
	options      = [], % Key/Value list of options
	uid          = 0,  % Unique Identifier
	flags        = [], % IMAP flags in proplist
	message      = []  % Whole Mail Message
}).

-record(mailbox_store, {
	name       = [], % Tuple {MailBoxName,UserName,DomainName}
	subscribed = false,
	uidnext     = 1,
	uidvalidity = 0,
	options    = [],
	messages   = []
}).

-record(message_store, {
	client  = [], % PID for FSM of the client
	server  = [], % node name of the server that has the mailbox open
	mailbox = [], % name of the mailbox
	state   = []  % current state of the mailbox; [open|active|closed]
}).

-record(erlmail_store, {
	system  = mnesia_store, 
	domain  = mnesia_store, 
	user    = mnesia_store, 
	message = mnesia_store, 
	mailbox = mnesia_store
}).
	
-define(SMTPD_PORT,25).
-define(SMTPD_MAX_CONN,25).
-define(SMTP_DATA_END, [13,10,46,13,10]). % End of data command "\r\n.\r\n"

-define(MAX_RESTART,    5).
-define(MAX_TIME,      60).
-define(TIMEOUT,   300000).

-record(smtpc, {
	socket = [],
	features = [],
	type = smtp, % smtp server type: [smtp:esmtp]
	state = helo % State of command, [helo,mail,rcpt,data]
}).

-record(smtpd_fsm, {
	socket      = [],
	addr        = [],
	relay       = false,
	options     = [],
	buff        = <<>>,
	line        = [],
	cmd         = undefined,
	param       = undefined,
	host        = undefined,
	mail        = undefined,
	rcpt        = undefined,
	to          = undefined,
	messagename = undefined,
	data        = undefined
}).

-record(smtpd_state, {
	listener,       % Listening socket
	acceptor,       % Asynchronous acceptor's internal reference
	'module'          % FSM handling module
}).

-record(outgoing_smtp,{
	rcpt       = [],
	tries      = 0,
	next_retry = [],
	response   = []
}).


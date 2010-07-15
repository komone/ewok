-module(ewok_imapd).

-include("ewok.hrl").
-include("ewok_system.hrl").
-include("email.hrl").

-behaviour(ewok_inet).
-export([start/1, init/2, terminate/3]).

-export([nonauthenticated/2, authenticated/2, selected/2, logout/2]).

-define(PROTOCOL_NAME, <<"IMAP4rev1">>).

%% Consider what to do about this...
-define(DOMAIN, <<"ewok">>).

-record(state, {remote_ip, remote_port}).

%%
start(Port) -> 
	#ewok_inet{
		id = ?MODULE,
		transport = tcp,
		port = Port,
		protocol = imap,
		handler = ?MODULE,
		codec = ewok_imap,
		timeout = 180
	}.

%%
init([], {RemoteIP, RemotePort}) ->
	State = #state{remote_ip=RemoteIP, remote_port=RemotePort},
	Reply = #imap_response{data = [{ok, [?PROTOCOL_NAME], <<"Service Ready">>}]},
	{reply, Reply, nonauthenticated, State}.

%%
%%nonauthenticated(#imap_request{id = ID, command = 'AUTHENTICATE', args = [<<"login">>|_]}, State) ->
%%	Reply = #imap_response{id = ID, status = ok, command = 'AUTHENTICATE', message = H},
%%	{reply, Reply, nonauthenticated, State};
nonauthenticated(#imap_request{id = ID, command = 'LOGIN', args = [Username, Password]}, State) ->
	case ewok_users:login(?DOMAIN, ewok_text:unquote(Username), ewok_text:unquote(Password)) of
	{ok, _User} ->
		Message = <<"User ", Username/binary, " authenticated">>, 
		Reply = #imap_response{id = ID, status = ok, command = 'LOGIN', message = Message},
		{reply, Reply, authenticated, State};
	{error, _Reason} ->
		Message = <<"Login denied">>,
		Reply = #imap_response{id = ID, status = no, command = 'LOGIN', message = Message},
		{reply, Reply, nonauthenticated, State}	
	end;
%
nonauthenticated(Message, State) ->
	anystate(Message, nonauthenticated, State).

%%
authenticated(#imap_request{id = ID, command = 'SELECT', args = _Args}, State) ->
	Data = [<<"0 EXISTS">>, <<"0 RECENT">>,
		<<"OK [UIDVALIDITY 3857529045] UIDs valid">>,
		<<"FLAGS (\Answered \Flagged \Deleted \Seen \Draft)">>,
		<<"OK [PERMANENTFLAGS (\Deleted \Seen \*)] Limited">>],	
	Reply = #imap_response{id = ID, command = 'SELECT', status = ok, data = Data},
	{reply, Reply, selected, State};

%{mailbox, [{exists, E},{recent, R},{flags, [answered, flagged, deleted, seen, draft]}


authenticated(Message, State) ->
	?TTY({authenticated, Message}),
	anystate(Message, authenticated, State).

%%
selected(#imap_request{id = ID, command = 'CLOSE'}, State) ->
	Reply = #imap_response{id = ID, command = 'CLOSE', status = ok},
	{reply, Reply, authenticated, State};

selected(Message, State) ->
	anystate(Message, selected, State).
	
%%
logout(#imap_request{id = ID, command = 'LOGOUT'}, State) ->
	Bye = {bye, [?PROTOCOL_NAME], <<"server terminating connection">>},
	Response = #imap_response{id = ID, command = 'LOGOUT', status = ok, data = [Bye]},
	{reply, Response, terminate, State}.
	
%%
anystate(Message = #imap_request{command = 'LOGOUT'}, _, StateData) ->
	logout(Message, StateData);
anystate(#imap_request{id = ID, command = 'NOOP', args = _Args}, State, StateData) ->
	Response = #imap_response{id = ID, status = ok, command = 'NOOP'},
	{reply, Response, State, StateData};
anystate(#imap_request{id = ID, command = 'CAPABILITY', args = _Args}, State, StateData) ->
	Capability = {'CAPABILITY', [?PROTOCOL_NAME], <<>>},
	Response = #imap_response{id = ID, status = ok, command = 'CAPABILITY', data = [Capability]}, 
	{reply, Response, State, StateData};
anystate(#imap_request{id = ID, command = Command}, State, StateData) ->
	Response = #imap_response{id = ID, status = bad, command = Command, message = <<"unrecognized command">>},
	{reply, Response, State, StateData}.

%%
terminate(_, _, _) ->
	ok.


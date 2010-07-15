-module(ewok_imap).
-include("ewok.hrl").
-include("email.hrl").

%% REF: http://tools.ietf.org/html/rfc3501

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

-compile(export_all).

-define(IMAP_PORT, 143).

%%%%%%%%%%
test() ->
	{ok, Q = #imap_request{id = ID, command = Command, args = _Args}} = decode(<<"a001 login mrc secret\r\n">>),
	?TTY(Q),
	
	{ok, R} = encode(#imap_response{id = ID, status = ok, command = Command, message = <<"completed">>}),
	?TTY(R).
test2() ->	
	Capability = {'CAPABILITY', [<<"IMAP4rev1">>], <<>>},
	encode(#imap_response{id = <<"a001">>, status = ok, command = 'CAPABILITY', data = [Capability]}).
%%%%%%%%%

%%
decode(Bin) ->
	[Line] = ewok_text:split(Bin, <<"[\r\n]+">>),
	[ID, Command | Args] = ewok_text:split(Line, ?SP),
	{ok, #imap_request{id = ID, command = command_key(Command), args = Args}}.


%-record(imap_response, {id, command, status, data = [], message}).
encode(#imap_response{id = ID, command = Command, status = Status, data = Data, message = Message}) ->
	StatusLine = encode_status(ID, Command, Status, Message),
	Untagged = encode_data(Data, []),
	Bin = list_to_binary(lists:reverse([StatusLine|Untagged])),
	{ok, Bin}.
%%
encode_status(undefined, _Command, _Status, _Message) ->
	<<>>;
encode_status(ID, Command, Status, Message) ->
	StatusMessage = 
		case {Status, Message} of
		{ok, undefined} -> <<"completed">>;
		{no, undefined} -> <<"failed">>;
		{bad, undefined} -> <<"error">>;
		_ -> Message
		end,
	list_to_binary([ID, ?SP, status_value(Status), ?SP, command_value(Command), ?SP, StatusMessage, ?CRLF]).
%%
encode_data([{Status, List, Message}|T], Acc) ->
	EncodedList = [[?SP, X] || X <- List],
	EncodedMessage = 
		case Message of
		<<>> -> Message;
		_ -> [?SP, Message]
		end,
	Line = list_to_binary([<<"* ">>, status_value(Status), EncodedList, EncodedMessage, ?CRLF]),
	encode_data(T, [Line|Acc]);
encode_data([H|T], Acc) ->
	Line = list_to_binary([<<"* ">>, H, ?CRLF]),
	encode_data(T, [Line|Acc]);
encode_data([], Acc) ->
	Acc. % don't reverse


%%
command_key(Value) when is_binary(Value) ->
	command(ewok_text:to_upper(Value)).
%% any state
command(<<"CAPABILITY">>)   -> 'CAPABILITY';
command(<<"NOOP">>)         -> 'NOOP';
command(<<"LOGOUT">>)       -> 'LOGOUT';
%% non-authenticated
command(<<"AUTHENTICATE">>) -> 'AUTHENTICATE';
command(<<"LOGIN">>)        -> 'LOGIN';
%% authenticated
command(<<"SELECT">>)       -> 'SELECT';
command(<<"EXAMINE">>)      -> 'EXAMINE';
command(<<"CREATE">>)       -> 'CREATE';
command(<<"DELETE">>)       -> 'DELETE';
command(<<"RENAME">>)       -> 'RENAME';
command(<<"SUBSCRIBE">>)    -> 'SUBSCRIBE';
command(<<"UNSUBSCRIBE">>)  -> 'UNSUBSCRIBE';
command(<<"LIST">>)         -> 'LIST';
command(<<"LSUB">>)         -> 'LSUB';
command(<<"STATUS">>)       -> 'STATUS';
command(<<"APPEND">>)       -> 'APPEND';
%% selected
command(<<"CHECK">>)        -> 'CHECK';
command(<<"CLOSE">>)        -> 'CLOSE';
command(<<"EXPUNGE">>)      -> 'EXPUNGE';
command(<<"SEARCH">>)       -> 'SEARCH';
command(<<"FETCH">>)        -> 'FETCH';
command(<<"STORE">>)        -> 'STORE';
command(<<"COPY">>)         -> 'COPY';
command(<<"UID">>)          -> 'UID';
%% EXPERIMENTAL
command(X = <<"X", _/binary>>) -> X.

command_value('CAPABILITY')   -> <<"CAPABILITY">>;  
command_value('NOOP')         -> <<"NOOP">>;        
command_value('LOGOUT')       -> <<"LOGOUT">>;      
command_value('AUTHENTICATE') -> <<"AUTHENTICATE">>;
command_value('LOGIN')        -> <<"LOGIN">>;       
command_value('SELECT')       -> <<"SELECT">>;      
command_value('EXAMINE')      -> <<"EXAMINE">>;     
command_value('CREATE')       -> <<"CREATE">>;      
command_value('DELETE')       -> <<"DELETE">>;      
command_value('RENAME')       -> <<"RENAME">>;      
command_value('SUBSCRIBE')    -> <<"SUBSCRIBE">>;   
command_value('UNSUBSCRIBE')  -> <<"UNSUBSCRIBE">>; 
command_value('LIST')         -> <<"LIST">>;        
command_value('LSUB')         -> <<"LSUB">>;        
command_value('STATUS')       -> <<"STATUS">>;      
command_value('APPEND')       -> <<"APPEND">>;      
command_value('CHECK')        -> <<"CHECK">>;       
command_value('CLOSE')        -> <<"CLOSE">>;       
command_value('EXPUNGE')      -> <<"EXPUNGE">>;     
command_value('SEARCH')       -> <<"SEARCH">>;      
command_value('FETCH')        -> <<"FETCH">>;       
command_value('STORE')        -> <<"STORE">>;       
command_value('COPY')         -> <<"COPY">>;        
command_value('UID')          -> <<"UID">>;         
command_value(X = <<"X", _/binary>>) -> X.

status(<<"OK">>)      -> ok;     
status(<<"NO">>)      -> no;     
status(<<"BAD">>)     -> bad;    

status(<<"PREAUTH">>) -> preauth;
status(<<"BYE">>)     -> bye.

status_value(ok)      -> <<"OK">>;
status_value(no)      -> <<"NO">>;
status_value(bad)     -> <<"BAD">>;

status_value(preauth) -> <<"PREAUTH">>;
status_value(bye)     -> <<"BYE">>;

status_value(X) -> command_value(X). %% TEMPORARY!!


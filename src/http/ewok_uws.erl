%% UBF Web Services (a.k.a. Universal Web Services)
-module(ewok_uws).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-compile(export_all).

-export([service_info/0]).

%% EXPERIMENTAL -- How do define a UBF Web Service...
-record(web_service, {id, 'vsn', contract=[]}).
%-record(name, {value}).
%-record('vsn', {value}).
-record(type, {id, def, info=[]}).
-record(state, {id, in, out, next}).
-record(event, {id, in}).
-record(anystate, {in, out}).

def() ->
	{?MODULE, fulfil, [integer]}.
	
request() ->
	request({?MODULE, fulfil, 3}).
	
request(MFA) ->
	Request = term_to_binary(MFA),
	Response = service(Request),
	Result = binary_to_term(Response),
	io:format("Request: ~p -> ~p~nResponse: ~p -> ~p~n", [MFA, Request, Response, Result]).

service(Request) ->
	case binary_to_term(Request) of
	{?MODULE, fulfil, X} when is_integer(X) ->
		Value = fulfil(X);
	_ ->
		Value = {error, enoent}
	end,
	term_to_binary(Value).

fulfil(Number) when is_integer(Number) ->
	ok.

%% NOTES

%% To define what approach to take we need to
%% first implement an application that offers a
%% UBF "web service" and then see what
%% generics can be pulled out.

%% Very likely this module will be largely used
%% for contract checking.

%% http://mydomain.com/?fun=directory

%% Fundamental types
int()      -> ewok_ubf:int(). % 'integer'
constant() -> ewok_ubf:constant(). % 'constant'
string()   -> ewok_ubf:string(). % 'string'
bin()      -> ewok_ubf:bin(). % 'binary'

%% EXPERIMENTAL -- How do define a UBF Web Service...
%% return a UBF(B) Contract (encoded as UBF(A))
%% try this with 'irc' from UBF(B) spec...
service_info() -> [#web_service{id=irc, 'vsn'="1.0", contract=contract()}].
%%
contract() -> [
	{type, info, info, ""}, 
	{type, description, description, ""}, 
	#type{id=contract, def=contract}, 
	
	{type, bool, [true, false], ""},
	{type, nick, string(), ""},
	{type, oldnick, string(), ""},
	{type, newnick, string(), ""},
	{type, group, string(), ""},
	{type, logon, logon, ""},
	{type, proceed, {ok, nick}, "A random nick is assigned"},
	{type, listGroups, groups, ""},
	{type, groups, [group], ""},
	{type, joinGroup, {join, group}, "You must join a group before you can send a message to it"},
	{type, leaveGroups, group, ""},
	{type, ok, ok, ""},
	{type, changeNick, {nick, nick}, "Change your nick in all groups"},
	{type, msg, {msg, group, string()}, "send a message to a group"},
	
	{type, msgEvent, {msg, nick, group, string()}, ""},
	{type, joinEvent, {joins, nick, group}, ""},
	{type, changesNameEvent, {changesName, oldnick, newnick, group}, ""},
	
	{state, start, logon, proceed, active},
	
	{state, active, listGroups, groups, active},
	{state, active, joinGroup, ok, active},
	{state, active, leaveGroup, ok, active},
	{state, active, changeNick, bool, active},
	#state{id=active, in=msg, out=bool, next=active},
	
	{event, active, msgEvent},
	{event, active, joinEvent},
	{event, active, leaveEvent},
	#event{id=active, in=changeNameEvent},
	
	{anystate, info, string()},
	{anystate, description, string()},
	#anystate{in=contract, out=term} %% term() seems to be undefined
].

json_protocols() -> "
data[getFile] = {fileName:string};
data[file] = {fileName:string, fileData:string};
Request = {msg:\"getFile\", data:{fileName:\"index.txt\"}}
Response = {msg:\"file\", data:{fileName:\"index.txt\", fileData:\"abc\"}, state:\"ready\"}

login x start -> challenge x wait;
response x wait -> ok x ready;
response x wait -> badpassword x stop;

data[login] = {name:string};
data[challenge] = {salt:string};
data[response] = {md5:string};

ready x listFiles -> files x ready;
ready x logout -> stop;

data[files] = [{filename:string}];

This completely (and formally specifies the behaviour of a file sever)
".

ubf_b_ref() -> "
+NAME(\"irc\").
+VSN(\"ubf1.0\").

+TYPES
info()         = info;
description()  = description;
contract()     = contract;

bool()         = true | false;
nick()         = string();
oldnick()      = string();
newnick()      = string();
group()        = string();
logon()        = logon;
proceed()      = {ok, nick()} \"A random nick is assigned\";
listGroups()   = groups;
groups()       = [group()];
joinGroup()    = {join, group()} 
	         \"You must join a group before you can send a message to it\";
leaveGroup()   = {leave, group()};
ok()           = ok;
changeNick()   = {nick, nick()} \"Change your nick in all groups\";
msg()          = {msg, group(), string()} \"send a message to a group\";
msgEvent()     = {msg, nick(), group(), string()};
joinEvent()    = {joins, nick(), group()};
leaveEvent()   =  {leaves, nick(), group()};
changeNameEvent() = {changesName, oldnick(),newnick(), group()}.

%% I am assigned an initial (random) nick

+STATE start logon() => proceed() & active. 

+STATE active
       
   listGroups() => groups() & active;  
   joinGroup()  => ok() & active;
   leaveGroup() => ok() & active;
   changeNick() => bool() & active; 
   msg()        => bool() & active;   % false if you have not joined the group

   EVENT => msgEvent();        % A group sends me a message
   EVENT => joinEvent();       % Nick joins group
   EVENT => leaveEvent();     % Nick leaves group
   EVENT => changeNameEvent(). % Nick changes name

+ANYSTATE
	info()        => string();
	description() => string();
	contract()    => term().
".

%%
%% File: ewok.hrl
%% Version: 1.0.0 beta
%% Author: Steve Davis <steve@simulacity.com>
%% Updated: July 30, 2009
%%
%% DON'T CHANGE THIS FILE WITHOUT COPYING TO INCLUDE/EWOK.HRL 
%% LATER: move all references to this file in the modules/source
%% to ../include/ewok.hrl and delete the copy in ./src

%% Definitions and records that may be used in applications

-define(SERVER_ID, <<"Ewok/1.0 BETA (Wicket)">>).
-define(VERSION, {1,0,0}).
-define(AUTHOR, 'steve@simulacity.com').

-define(CONFIG_FILE_EXT, ".conf").
-define(ARCHIVE_FILE_EXT, ".ez").

-define(ESP_FILE_EXT, ".esp").
-define(LOG_FILE_EXT, ".log").
-define(LOG_ARCHIVE_EXT, ".tar.gz").

%% Debugging use only. Remove all usage instances at release.
%% Do not use io:format directly in the source code as this macro 
%% will make finding and removing spurious io: messages harder.
%% Currently, commenting out this macro will allow us to find
%% all development console messages in the code very easily.
-define(TTY(Format, Args), io:format(user, Format, Args)).

%% Only semi-valid. There just *has* to be a better way :(
-define(is_string(S), (is_list(S) andalso S =/= [] andalso is_integer(hd(S)))).
%% This is another semi-valid guard. NOTE: used instead of is_record/2 when runtime type isn't known.
-define(is_record(X), is_tuple(X) andalso is_atom(element(1, X)) andalso size(X) > 1).

%% This record is not (currently) used; It is more a statement of intent and a
%% possible semantic approach to "strings" that more closely observes genuine 
%% engineering principles and standards. You can safely ignore this record for now.
-record(text, {bin= <<>>, charset=utf8, lang='us-en'}).

%% The 'task' record is the API to the scheduler
%% Meaning: "Using this task 'id' as a reference, execute this 'function' every 'repeat' 
%% seconds, and 'notify' this pid -- usually self() or 'undefined' -- when the task is 
%% executed; 'start' this task either 'now' or at int() in gregorian seconds universal time. 
%% If 'repeat' is 'once' then only execute this task once at 'start' and then drop it. 
%% The value of 'timer_ref' is reserved and allocated/used by the scheduler (ewok_scheduler_srv).
-record(task, {id, function, start=now, repeat=once, terminate=infinity, notify, timer_ref}).

%% Ewok Session Management
-define(EWOK_SESSION_KEY, <<"_EWOKSID">>).
-record(ewok_session, {key, ip, user, data=[], started, expires, ttl, notify}).

%% Ewok User Management
-record(user, {id, name, email, roles=[]}). %% TODO: move 'email' to profile.
-record(profile, {id, attributes=[]}). %% KV store for profile attributes
-record(role, {id, info=[]}). %% Not an API - Should this could be put into system.hrl?
%% Auth and security... not intended for API usage. Could/should this record could be 
%% move into ewok_system.hrl?
-record(auth, {id, password, activation}).

%% NOTE: The only exposure to the API currently is in the .web configuration,
%% and so this record may be hidden/moved to ewok_system.hrl.
%% The url 'path', is always relative to www root "/", and passes execution 
%% to the module 'handler' which must implement the callbacks defined by the 
%% behaviour 'ewok_http_resource'. The resource is always associated with a
%% security/application 'realm' which is usually the web application id.
%% Access to the resource is constrained to the defined 'roles'. The allowed roles 
%% may also be roles defined by other realms and identified as {Realm, Role}.
%% NOTE By default, 'roles' = [], which means that there is *no access allowed*. 
%% To offer public access to the resource, 'roles' should be set to 'any'.
-record(route, {path, handler, realm, roles=[]}).

%% Could this could be put in system.hrl?
-record(mimetype, {ext=[], media=[]}).
%% Could this could be put in system.hrl?
-record(response, {status, headers=[], content=[], close=false}).

%% Fallback defaults used internally by the server to deal with missing keys from .web files.
%% These may be REMOVED when full validation in ewok_configuration is stable. 
%% NOTE:....is this a good pattern for internal defaults... or not?
-define(DEFAULT_WWW_ROOT, "./priv/www").
-define(DEFAULT_WWW_INDEX_FILE, "index.html").
-define(DEFAULT_APP_ROOT, ".priv/apps").
-define(KEYSTORE_FILE, ".keystore").
%% end %%

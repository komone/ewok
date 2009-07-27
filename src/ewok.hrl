%% DON'T CHANGE THIS WITHOUT COPYING TO INCLUDE/EWOK.HRL 
-define(SERVER_ID, <<"Ewok/1.0 (Wicket)">>).
-define(VERSION, {1, 0, 0}).
-define(AUTHOR, 'steve@simulacity.com').

-define(CONFIG_FILE_EXT, ".web").
-define(LOG_FILE_EXT, ".log").
-define(ESP_FILE_EXT, ".esp").

-define(TTY(Format, Args), io:format(Format, Args)).

-define(is_string(S), (is_list(S) andalso S =/= [] andalso is_integer(hd(S)))).

%% The url 'path', which is always relative to www root "/", passes execution 
%% to the module 'handler' which must implement the callbacks defined by the 
%% behaviour 'ewok_http_resource'. The resource is always associated with a
%% 'realm' which is usually the web application name.
%% Access to the resource is constrained to the defined 'roles'. Allowed roles 
%% may be roles defined by other realms and identified as {Realm, Role}.
%% By default, 'roles' = [], which means that there is no access allowed. 
%% To offer public access to the resource, 'roles' should be set to 'any'.
-record(route, {path, handler, realm, roles=[]}).
%%
-record(mimetype, {ext=[], media=[]}).
%%
-record(response, {status, headers=[], content=[], close=false}).

%% The 'task' record:
%% Using this task 'id' as a reference, execute this 'function' every 'repeat' seconds,
%% and 'notify' this pid (usually self() or undefined) when the task is executed, 
%% 'start' this task either 'now' or an int() of gregorian seconds universal time. 
%% If 'repeat' is 'once' then only execute this task once at 'start' and then drop it. 
%% The value of 'timer_ref' is reserved and allocated/used by the scheduler.
-record(task, {id, function, start=now, repeat=once, terminate=infinity, notify, timer_ref}).

%% Ewok User Management
-record(role, {id, info=[]}).
-record(user, {id, name, email, roles=[]}).
-record(auth, {id, password, activation}). % keep these here and don't expose in the session 
-record(profile, {id, attributes=[]}).

% Fallback defaults used internally by the server to deal with missing keys from .web files.
% These will be REMOVED when full validation in ewok_config is stable. 
-define(DEFAULT_WWW_ROOT, "./priv/www").
-define(DEFAULT_WWW_INDEX_FILE, "index.html").


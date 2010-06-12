%% Copyright 2010 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

%% ewok_system.hrl
-vsn("1.0.0").
-author('steve@simulacity.com').

-define(SERVER_ID, <<"Ewok/1.0 BETA (Wicket)">>).

%% Defaults used internally by the server on installation
%% These may be overridden by corresponding keys in the .app env entry.
-define(DATA_DIR, <<"./priv/data">>).
-define(LOG_DIR, <<"./priv/log">>).
-define(BOOT_LOG, <<"./boot.log">>).

-define(APP_ROOT, <<"./priv/apps">>).
-define(WWW_ROOT, <<"./priv/www">>).
-define(WWW_INDEX_FILE, <<"index.html">>).

-define(KEYSTORE_FILE, <<".keystore">>).

-define(ADMIN_USER, {ewok, <<"admin">>}).
-define(ADMIN_ROLE, {ewok, admin}).

-define(EWOK_SESSION_KEY, <<"_SESSION">>).


%% These are records that need to be shared between services but they should
%% not be required by the API. Records required by the API are in ewok.hrl.
-record(ewok_config, {key, value}).

%% Used to initialize internet services
-record(ewok_inet, {
	name, 
	ip = {0, 0, 0, 0}, 
	port = 0, 
	timeout = infinity, 
	max_connections = infinity, 
	transport = gen_tcp, 
	protocol, 
	handler, 
	codec,
	socket_opts = []
}).

-record(ewok_socket, {
	ip = {0,0,0,0},
	mode = binary,
	packet = 0,
	active = false,
	backlog = 0,
	nodelay = true,
	reuseaddr = true,
	recbuf = 8192
}).

%% The 'task' record is the API to the scheduler
%% Meaning: "Using this task 'id' as a reference, execute this 'function' every 'repeat' 
%% seconds, and 'notify' this pid -- usually self() or 'undefined' -- when the task is 
%% executed; 'start' this task either 'now' or at int() in gregorian seconds universal time. 
%% If 'repeat' is 'once' then only execute this task once at 'start' and then drop it. 
%% The value of 'timer_ref' is reserved and allocated/used by the scheduler (ewok_scheduler_srv).
-record(ewok_task, {id, function, start=now, repeat=once, terminate=infinity, notify, timer_ref}).
%%
-record(server, {runmode, path, apps=[]}).
%%
-record(web_app, {id, path, config, valid=false, deployed=false}).
%%
-record(datasource, {id, name, mod, running=false, valid=false, data=[]}).
%%
-record(ewok_log, {id, fd, path, rollover=infinity, maxfiles=infinity}).
%%
-record(workflow, {id, name, workitems=[]}).
%% TODO: a 'task' record for workflow would conflict with the scheduler's task record
-record(workitem, {queue, id, roles, payload}).
%% Auth and security... not intended for API usage. 
-record(ewok_auth, {id, password, activation}).

%% The url 'path', is always relative to www root "/", and passes execution 
%% to the module 'handler' which must implement the callbacks defined by the 
%% behaviour 'ewok_http_resource'. The resource is always associated with a
%% security/application 'realm' which is usually the web application id.
%% Access to the resource is constrained to the defined 'roles'. The allowed roles 
%% may also be roles defined by other realms and identified as {Realm, Role}.
%% NOTE By default, 'roles' = [], which means that there is *no access allowed*. 
%% To offer public access to the resource, 'roles' should be set to 'any'.
-record(ewok_route, {path, handler, realm, roles=[]}).
%%
-record(ewok_session, {key, ip, user, data=[], started, expires, ttl, notify}).
%%
-record(ewok_user, {id, name, roles=[]}). %% 
%%
-record(ewok_role, {id, info=[]}). %% 
%%
-record(ewok_profile, {id, attributes=[]}). %% KV store for profile attributes
%% 
-record(ewok_mimetype, {ext=[], media=[]}).
%%
-record(response, {status, headers=[], content=[], close=false}).
%%
-record(ewok_file, {route, file, mimetype, modified, size=0, bin= <<>>}).

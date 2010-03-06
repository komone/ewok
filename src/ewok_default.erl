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

-module(ewok_default).

-compile(export_all).

% ewok.conf
% all time values are in seconds
% all size values are in bytes
config() -> [
	{{server, ip}, any},
	{{server, hostname}, <<"localhost">>}, %% unused
	{{server, runmode}, development},
	{identity, [
		{keystore, <<"./priv/data/.keystore">>},
		{password, <<"password">>}
	]},	
	{tasks, []}, % no conf as yet
	{{datasource, default}, mnesia}, % no other option offered for now (and maybe never)
	{{datasource, mnesia}, [
		{mod, ewok_mnesia_ds}, %%
		{args, []},
		{path, <<"./priv/data">>}
	]},
	{{datasource, riak}, [
		{mod, riak_ds},
		{args, []}
	]},
	{{datasource, couchdb}, [
		{mod, couchdb_ds},
		{args, []}
	]},
	{{datasource, postgresql}, [
		{mod, postgresql_ds},
		{args, []}
	]},
	{{datasource, mysql}, [
		{mod, mysql_ds},
		{args, []}
	]},
	{{datasource, aws_sdb}, [
		{mod, aws_sdb_ds}, 
		{args, []},
		%% NOTE: sample keys from Amazon - move to .keystore later
		{sdb_access_key, <<"022QF06E7MXBSH9DHM02">>}, 
		{sdb_secret_key, <<"kWcrlUX5JEDGM/LtmEENI/aVmYvHNif5zB+d9+ct">>}
	]},
	{{queue, default}, rabbitmq}, % no other option offered for now (and maybe never)
	{{queue, rabbitmq}, [
		{data_path, <<"./priv/data">>},
		{url, <<"amqp://localhost">>},
		{port, 5432},
		{user, <<"admin">>},
		{password, <<"password">>}
	]},
	{{umtp, socket}, [
		{port, 30},
		{request_timeout, 10},
		{max_connections, 2048},
		{mode, binary},
		{reuseaddr, true},
		{packet, 0}, % don't think this is exactly the same as 'raw'
		{active, false},
		{recbuf, 8192},
		{backlog, 30},
		{nodelay, true},
		{ssl, [
			{enabled, false},
			{verify, 0}, %% for now, direct representation of SSL opts
			{depth, 1}, %% for now, direct representation of SSL opts
			{password, <<"">>}, %% you should ONLY set this if key.pem is pw protected
			{keyfile, <<"./priv/ssl/key.pem">>},
			{certfile, <<"./priv/ssl/cert.pem">>},
			{cacertfile, <<"./priv/ssl/cacerts.pem">>}
		]}
	]},
	{{smtp, socket}, [
		{port, 25},
		{request_timeout, 30},
		{max_connections, 20},
		{mode, binary},
		{reuseaddr, true},
		{packet, 0}, % don't think this is exactly the same as 'raw'
		{active, false},
		{recbuf, 8192},
		{backlog, 0},
		{nodelay, true},
		{ssl, [
			{enabled, false},
			{verify, 0}, %% for now, direct representation of SSL opts
			{depth, 1}, %% for now, direct representation of SSL opts
			{password, <<"">>}, %% you should ONLY set this if key.pem is pw protected
			{keyfile, <<"./priv/ssl/key.pem">>},
			{certfile, <<"./priv/ssl/cert.pem">>},
			{cacertfile, <<"./priv/ssl/cacerts.pem">>}
		]}
	]},
	{{http, socket}, [
		{port, 8080},
		{max_connections, 2048},
		{mode, binary},
		{reuseaddr, true},
		{packet, 0}, % don't think this is exactly the same as 'raw'
		{active, false},
		{backlog, 30},
		{recbuf, 8192},
		{nodelay, true},
		{ssl, [
			{enabled, false},
			{verify, 0}, %% for now, direct representation of SSL opts
			{depth, 1}, %% for now, direct representation of SSL opts
			{password, ""}, %% you should ONLY set this if key.pem is pw protected
			{keyfile, "./priv/ssl/key.pem"},
			{certfile, "./priv/ssl/cert.pem"},
			{cacertfile, "./priv/ssl/cacerts.pem"}
		]}
	]},
	{{http, header_limit}, 100}, %% 'infinity' turns off...
	{{http, request_timeout}, 30},
	{{http, deploy_root}, "./priv/apps"},
	{{http, autodeploy}, [ewok_admin]},
		
	%% Cache is used in production mode to cache static files
	%% and their file information for response headers --
	%% the idea is to reduce file handles used by the system
	%% and potentially massively speed up static file serving
	%% "RAM is the new Disk" -- however you are well advised to
	%% shove all non-sensitive file resources into your downstream
	%% http server where possible and let that server do the work.
	{{http, cache, max_file_size}, 102400}, % up to 100k - depends on your RAM
	{{http, session, timeout}, 1800}, %% http sessions timeout after 30 minutes
	{{http, session, flush_interval}, 120}, %% flush sessions every 2 minutes when idle
	{{http, session, force_flush}, 3600} %% force a cleanup of stale sessions every hour
].

mimetypes() -> [
	{default, "application/x-octet-stream"},
	{".bmp", "image/bmp"},
	{".bz2", "application/x-bzip2"},
	{".css", "text/css"},
	{".csv", "text/csv"},
	{".doc", "application/msword"},
	{".esp", "application/xhtml+xml"},
	{".exe", "application/octet-stream"},
	{".gif", "image/gif"},
	{".gz", "application/x-gzip"},
	{".html", "text/html"},
	{".ico", "image/x-icon"},
	{".jpg", "image/jpeg"},
	{".js", "application/x-javascript"},
	{".json", "application/json"},
	{".m3u", "audio/x-mpegurl"},
	{".m4a", "audio/mpeg"},
	{".mov", "video/quicktime"},
	{".mp3", "audio/mpeg"},
	{".pdf", "application/pdf"},
	{".png", "image/png"},
	{".rtf", "application/rtf"},
	{".swf", "application/x-shockwave-flash"},
	{".tar", "application/x-tar"},
	{".tgz", "application/x-gzip"},
	{".txt", "text/plain"},
	{".ubf", "application/x-ubf"},
	{".xhtml", "application/xhtml+xml"},
	{".xls", "application/vnd.ms-excel"},
	{".xml", "application/xml"},		
	{".wav", "audio/x-wav"},
	{".z", "application/x-compress"},
	{".zip", "application/zip"}
].

log() -> [
	%% TODO: if this is moved to http, log then there's a dependency
	%% to be accounted for in create_log in ewok_logging_srv
	{{log, level}, info},
	{{log, path}, <<"./priv/log">>},
	
	{{log, access, enable}, true},
	{{log, access, rollover}, infinity},
	{{log, access, maxfiles}, infinity},
	
	{{log, auth, enable}, true},
	{{log, auth, rollover}, infinity},
	{{log, auth, maxfiles}, infinity},
	
	{{log, debug, enable}, true},
	{{log, debug, rollover}, infinity},
	{{log, debug, maxfiles}, infinity},
	
	{{log, mail, enable}, true},
	{{log, mail, rollover}, infinity},
	{{log, mail, maxfiles}, infinity},
	
	{{log, server, enable}, true},
	{{log, server, rollover}, infinity},
	{{log, server, maxfiles}, infinity}
].

%% this will probably disappear when web_admin is finished...
%% ...or perhaps be used *only* for installation.
web_application() -> {ewok, [
	{http, [
		{app_path, <<"/">>}, % is this ever necessary ???
		
		%% realm-based role... this probably shouldn't be in here at all
		{login, <<"/login">>}, 
		
		{doc_root, <<"./priv/www">>},
		{template_root, <<"./priv/esp">>},
		{index_file, <<"index.html">>},
		
		{roles, []},
		
		{route, default, ewok_file_handler, ewok, any},
		{route, <<"/">>, ewok_home, ewok, any},
		{route, <<"/app/login">>, ewok_world, ewok, any},
		{route, <<"/cgi-bin/login.cgi">>, ewok_world, ewok, any},
		{route, <<"/home">>, ewok_home, ewok, any},
		{route, <<"/ajax">>, ewok_print_handler, ewok, any},
		{route, <<"/login">>, ewok_login_handler, ewok, any},
		{route, <<"/activation">>, ewok_activation_handler, ewok, any},
		{route, <<"/registration">>, ewok_registration_handler, ewok, any},
		{route, <<"/websocket">>, ewok_websocket_handler, ewok, any}
	]}	
]}.

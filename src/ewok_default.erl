-module(ewok_default).

-compile(export_all).

% ewok.conf
% all time values are in seconds
% all size values are in bytes
config() -> [
	{{server, ip}, any},
	{{server, hostname}, "localhost"}, %% unused
	{{server, runmode}, development},
		
	{{identity, keystore}, "./priv/data/.keystore"},
	{{identity, password}, "password"},
	
	{tasks, []}, % no conf as yet
	
	{{datasource, default}, mnesia}, % no other option offered for now (and maybe never)
	{{datasource, mnesia, mod}, ewok_mnesia_ds}, %%
	{{datasource, mnesia, args}, []},
	{{datasource, mnesia, path}, "./priv/data"},
	{{datasource, riak, mod}, riak_ds},
	{{datasource, riak, args}, []},
	{{datasource, couchdb, mod}, couchdb_ds},
	{{datasource, couchdb, args}, []},
	{{datasource, postgresql, mod}, postgresql_ds},
	{{datasource, postgresql, args}, []},
	{{datasource, mysql, mod}, mysql_ds},
	{{datasource, mysql, args}, []},

	{{datasource, aws_sdb, mod}, aws_sdb_ds}, 
	{{datasource, aws_sdb, args}, []},
	{{datasource, aws_sdb, args}, []},
	%% NOTE: sample keys from Amazon - move to .keystore later
	{{datasource, aws_sdb, sdb_access_key}, "022QF06E7MXBSH9DHM02"}, 
	{{datasource, aws_sdb, sdb_secret_key}, "kWcrlUX5JEDGM/LtmEENI/aVmYvHNif5zB+d9+ct"},
	
	{{queue, default}, rabbitmq}, % no other option offered for now (and maybe never)
	{{queue, rabbitmq, data_path}, "./priv/data"},
	{{queue, rabbitmq, url}, "amqp://localhost"},
	{{queue, rabbitmq, port}, 5432},
	{{queue, rabbitmq, user}, "admin"},
	{{queue, rabbitmq, password}, "password"},
	
	{{umtp, port}, 30},
	{{umtp, request_timeout}, 10},
	{{umtp, tcp, max_connections}, 2048},
	{{umtp, tcp, socket, mode}, binary},
	{{umtp, tcp, socket, reuseaddr}, true},
	{{umtp, tcp, socket, packet}, 0}, % don't think this is exactly the same as 'raw'
	{{umtp, tcp, socket, active}, false},
	{{umtp, tcp, socket, recbuf}, 8192},
	{{umtp, tcp, socket, backlog}, 30},
	{{umtp, tcp, socket, nodelay}, true},
	{ssl, [
		{enabled, false},
		{verify, 0}, %% for now, direct representation of SSL opts
		{depth, 1}, %% for now, direct representation of SSL opts
		{password, ""}, %% you should ONLY set this if key.pem is pw protected
		{keyfile, "./priv/ssl/key.pem"},
		{certfile, "./priv/ssl/cert.pem"},
		{cacertfile, "./priv/ssl/cacerts.pem"}
	]},	
	
	{{smtp, port}, 25},
	{{smtp, request_timeout}, 30},
	{{smtp, tcp, max_connections}, 20},
	{{smtp, tcp, socket, mode}, binary},
	{{smtp, tcp, socket, reuseaddr}, true},
	{{smtp, tcp, socket, packet}, 0}, % don't think this is exactly the same as 'raw'
	{{smtp, tcp, socket, active}, false},
	{{smtp, tcp, socket, recbuf}, 8192},
	{{smtp, tcp, socket, backlog}, 0},
	{{smtp, tcp, socket, nodelay}, true},
	{ssl, [
		{enabled, false},
		{verify, 0}, %% for now, direct representation of SSL opts
		{depth, 1}, %% for now, direct representation of SSL opts
		{password, ""}, %% you should ONLY set this if key.pem is pw protected
		{keyfile, "./priv/ssl/key.pem"},
		{certfile, "./priv/ssl/cert.pem"},
		{cacertfile, "./priv/ssl/cacerts.pem"}
	]},
	
	{{http, port}, 8080},
	{{http, tcp, max_connections}, 2048},
	{{http, tcp, socket, mode}, binary},
	{{http, tcp, socket, reuseaddr}, true},
	{{http, tcp, socket, packet}, 0}, % don't think this is exactly the same as 'raw'
	{{http, tcp, socket, active}, false},
	{{http, tcp, socket, backlog}, 30},
	{{http, tcp, socket, recbuf}, 8192},
	{{http, tcp, socket, nodelay}, true},
	
	{ssl, [
		{enabled, false},
		{verify, 0}, %% for now, direct representation of SSL opts
		{depth, 1}, %% for now, direct representation of SSL opts
		{password, ""}, %% you should ONLY set this if key.pem is pw protected
		{keyfile, "./priv/ssl/key.pem"},
		{certfile, "./priv/ssl/cert.pem"},
		{cacertfile, "./priv/ssl/cacerts.pem"}
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
	{{log, path}, "./priv/log"},
	
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
		{app_path, "/"}, % is this ever necessary ???
		
		%% realm-based role... this probably shouldn't be in here at all
		{login, "/login"}, 
		
		{www_root, "./priv/www"},
		{template_root, "./priv/esp"},
		{index_file, "index.html"},
		
		{roles, []},
		
		{route, default, ewok_file_handler, ewok, any},
		{route, "/", ewok_home, ewok, any},
		{route, "/app/login", ewok_world, ewok, any},
		{route, "/cgi-bin/login.cgi", ewok_world, ewok, any},
		{route, "/home", ewok_home, ewok, any},
		{route, "/ajax", ewok_print_handler, ewok, any},
		{route, "/login", ewok_login_handler, ewok, any},
		{route, "/activation", ewok_activation_handler, ewok, any},
		{route, "/registration", ewok_registration_handler, ewok, any},
		{route, "/websocket", ewok_websocket_handler, ewok, any}
	]}	
]}.

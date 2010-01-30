{application, ewok, [
	{description, "EWOK SIM 100 Beta"},
	{vsn, "1.0.0"},
	{modules, [
		ewok, ewok_app, ewok_cache, ewok_cache_srv, ewok_config, ewok_data_srv,
		ewok_db, ewok_deployment_srv, ewok_http_srv, ewok_identity, ewok_log, 
		ewok_scheduler_srv, ewok_session_srv, ewok_sup, ewok_tcp_srv, 
		ewok_util, ewok_workflow_sup, ewok_smtp_srv 
	]},
	{registered, [
		ewok_sup,
		ewok_cache_srv,
		ewok_data_srv,
		ewok_deployment_srv,
%		ewok_geoip_srv, % no longer...!
		ewok_http_srv,
		ewok_scheduler_srv,
		ewok_session_srv,
		ewok_smtp_srv,
		ewok_umtp,
		ewok_workflow_sup
	]},
	
	%% The args for ewok_sup control service load order. Don't change this 
	%% order unless you are sure that you know what you are doing!
	{mod, {ewok_app, [
		ewok_identity_srv,
		ewok_scheduler_srv,
		ewok_cache_srv, % non-transactional utility cache
		ewok_data_srv, % external datasources
		ewok_session_srv,
%		ewok_workflow_sup,
		ewok_deployment_srv,
		ewok_http_srv
%		ewok_smtp_srv,
%		ewok_umtp
	]}},
	%% 
	{env, [
		{autoinstall, true}, %% move into server term?
		{runmode, development},
		{autodeploy, [admin]},
%		{server, [
%			{ip, any},
%			{hostname, "localhost"}, %% unused
%			{runmode, development},
		%% attempt to correctly install ewok when the application is 
		%% started, if the install doesn't validate. If this is 'false'
		%% then the call ewok:install() must be made before running the
		%% application server.
%			{boot_log, "./boot.log"}, 
%			{log_dir, "./priv/log"},
%			{data_dir, "./priv/data"}
%		]},
		{web_app, [
			{app_path, "/"}, % used for static file urls
			
			%% realm-based role... this probably shouldn't be in here at all
			{login, "/login"}, 
			
			{www_root, "./priv/www"},
			{template_root, "./priv/esp"},
			{index_file, "index.html"},
			
			{roles, []},
			%% -record(route, {path, handler, realm, roles=[]}).
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
		]},
		{mimetypes, [
			%% maybe move to a seperate def file... or as a separate term
			{default, "application/x-octet-stream"},
			{".bmp",  "image/bmp"},
			{".bz2",  "application/x-bzip2"},
			{".css",  "text/css"},
			{".csv",  "text/csv"},
			{".doc",  "application/msword"},
			{".esp",  "application/xhtml+xml"},
			{".exe",  "application/octet-stream"},
			{".gif",  "image/gif"},
			{".gz",   "application/x-gzip"},
			{".html", "text/html"},
			{".ico",  "image/x-icon"},
			{".jpg",  "image/jpeg"},
			{".js",   "application/x-javascript"},
			{".json", "application/json"},
			{".m3u",  "audio/x-mpegurl"},
			{".m4a",  "audio/mpeg"},
			{".mov",  "video/quicktime"},
			{".mp3",  "audio/mpeg"},
			{".pdf",  "application/pdf"},
			{".png",  "image/png"},
			{".rtf",  "application/rtf"},
			{".swf",  "application/x-shockwave-flash"},
			{".tar",  "application/x-tar"},
			{".tgz",  "application/x-gzip"},
			{".txt",  "text/plain"},
			{".ubf",  "application/x-ubf"},
			{".xhtml", "application/xhtml+xml"},
			{".xls",  "application/vnd.ms-excel"},
			{".xml",  "application/xml"},		
			{".wav",  "audio/x-wav"},
			{".z",    "application/x-compress"},
			{".zip",  "application/zip"}
		]}
	]},
	{applications, [kernel, stdlib]} 
]}.

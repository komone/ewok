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
		ewok_geoip_srv, % test
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
		ewok_cache_srv,
		ewok_scheduler_srv,
		ewok_data_srv,
		ewok_identity_srv,
		ewok_session_srv,
%		ewok_workflow_sup,
		ewok_deployment_srv,
		ewok_http_srv
%		ewok_smtp_srv,
%		ewok_umtp
	]}},
	%% 
	{env, [
		%% attempt to correctly install ewok when the application is 
		%% started, if the install doesn't validate. If this is 'false'
		%% then the call ewok:install() must be made before running the
		%% application server.
		{autoinstall, true}, 
		%% i.e. call the log boot.log and place in current directory
		%% don't rollover (i.e. overwrite on each boot.
		{boot, [
			{log, boot}, %% i.e. boot.log
			{path, "."}, %% in the current directory
			{rollover, false} 
				%% false - overwrite last bootfile
				%% true - archive (rollover) last bootfile
		]},
		{web_config, [{file, "ewok.conf"}]}
	]},
	{applications, [kernel, stdlib]} 
]}.

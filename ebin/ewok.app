{application, ewok, [
	{description, "EWOK SIM 100 Beta"},
	{vsn, "1.0.0"},
	{modules, [
		esp_workflow_sup, ewok, ewok_cache, ewok_cache_srv,
		ewok_config, ewok_data_srv, ewok_db, ewok_deployment_srv,
		ewok_http_srv, ewok_identity, ewok_log, ewok_scheduler_srv,
		ewok_session_srv, ewok_sup, ewok_tcp_srv, ewok_util
	]},
	{registered, [
		ewok_sup,
		ewok_data_srv,
		ewok_cache_srv,
		ewok_scheduler_srv,
		ewok_deployment_srv,
		ewok_session_srv,
		ewok_http_srv,
		ewok_geoip_srv, % test
		esp_workflow_sup
	]},
	
	%% The args for ewok_sup control service load order. Don't change this 
	%% order unless you are sure that you know what you are doing!
	{mod, {ewok_sup, [
		cache, 
		datasource, 
		scheduler, 
		deployer, 
		workflow, 
		session, 
		http
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
			{rollover, false} %% overwrite
		]}
	]},
	{applications, [kernel, stdlib]} 
]}.

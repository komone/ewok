{application, admin, [
	{description, "Ewok Web Administration"},
	{vsn, "1.0.0"},
	{modules, [
		ewok_admin,
		ewok_admin_web 
	]},
	{registered, []},
	{applications, [ewok]},
	%% {mod, {ewok_admin, []}},
	{env, [
		{web_app, [
			{name, "Ewok Web Admin"},
			{app_path, "/admin"}, %% ???
			{doc_root, "./priv/www"},
			{template_root, "./priv/esp"},
			{login, "/admin/login"},
			{roles, [{ewok,admin}]},
			{route, "/admin/*", ewok_web_admin, ewok, [{ewok, admin}]},
			{route, "/admin/login", ewok_login_handler, ewok, any}
		]}
	]}
]}.

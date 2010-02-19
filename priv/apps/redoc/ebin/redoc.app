{application, redoc, [
	{description, "Ewok Documentation"},
	{vsn, "1.0.0"},
	{modules, [
		redoc,
		esp_src2html
	]},
	{registered, []},	
	%% 
	{env, [
		{web_app, [
			{app_path, "/doc"}, %%
			{doc_root, "./priv/www"},
			{template_root, "./priv/esp"},
			{roles, [admin]}
			{route, "/doc", redoc_handler, any, any}
		]}
	]}
]}.

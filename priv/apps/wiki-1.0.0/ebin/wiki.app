{application, wiki, [
	{description, "Wiki Application"},
	{vsn, "1.0.0"},
	{modules, [
		wiki
	]},
	{registered, []},	
	%% 
	{mod, {wiki, []}},
	{env, [
		{web_app, [
			{app_path, "/doc"}, %%
			{doc_root, "./priv/www"},
			{template_root, "./priv/esp"},
			{roles, [admin, user]},
			{route, "/wiki/*", wiki_web, any, any}
		]}
	]}
]}.

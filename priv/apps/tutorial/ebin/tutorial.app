{application, tutorial, [
	{description, "Tutorial Application"},
	{vsn, "1.0.0"},
	{modules, [ 
		tutorial,
		exercise1,
		exercise2,
		exercise3,
		exercise4,
		exercise5,
		exercise6
	]},
	{registered, []},	
	{env, [
		{web_app, [
			{name, "Tutorial Application"},
			{app_path, "/tutorial"}, %???
			{doc_root, "./priv/www"},
			{template_root, "./priv/esp"},
			{roles, []},
			
			{route, "/tutorial/ex1", exercise1, tutorial, any},
			{route, "/tutorial/ex2", exercise2, tutorial, any},
			{route, "/tutorial/ex3", exercise3, tutorial, any},
			{route, "/tutorial/ex4", exercise4, tutorial, any},
			{route, "/tutorial/ex5", exercise5, tutorial, any},
			{route, "/tutorial/ex6", exercise6, tutorial, any},
			{route, "/tutorial/*", ewok_file_handler, tutorial, any}
		]}
	]}
]}.

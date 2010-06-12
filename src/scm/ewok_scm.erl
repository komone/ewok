-module(ewok_scm).
-name("Ewok Source Control").

-compile(export_all).

-define(GIT, "\"C:/Program Files/Git/bin/git.exe\"").
-define(REPO_PATH, "D:/Erlang/lib/").

decode(Bin) ->
	ok.
encode(Term) ->
	ok.
	
test(Repo) ->
	RepoDir = ?REPO_PATH ++ Repo,
	case filelib:is_dir(RepoDir) of
	true ->
		Command = ?GIT ++ " upload-pack " ++ RepoDir,
		Port = erlang:open_port({spawn, Command}, [binary]),
		io:format("Info: ~p~n", [erlang:port_info(Port)]),
		response(Port);
	false ->
		no_repo
	end.
	
response(Port) ->
	receive
	{Port, {data, Data}} ->
		io:format("Data: ~p~n", [Data]),
%%		erlang:port_command(Port, Data),
		{ok, Port};
    Msg ->
		error_logger:error_msg("unknown message ~p~n", [Msg]),
		{ok, Port}
    after 5000 ->
		error_logger:error_msg("timed out waiting for port~n")
	end.
  

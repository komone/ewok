%% Copyright 2009-2010 Steve Davis <steve@simulacity.com>
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%% 
%% http://www.apache.org/licenses/LICENSE-2.0
%% 
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.

-module(ewok_gitd).
-name("Ewok Source Code Management Service").

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_inet).
-export([init/2, terminate/3]).
-export([data/2]).

-export([start/0, start/1]).

%% temp
-export([test/1]).

-compile(export_all).
-define(SCM_PORT, 9418).

-define(GIT, "\"C:/Program Files/Git/bin/git.exe\"").
-define(REPO_PATH, "D:/Erlang/lib/").

-record(state, {id, cmd, from, to=[], body=[]}).

start() ->
	start(?SCM_PORT).
	
start(Port) ->
	#ewok_inet{
		protocol = git,
		transport = tcp,
		port = Port,
		handler = ewok_scmd,
		timeout = 120
	}.


%%%
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

init(Options, _Remote) ->
	RepoDir = proplists:get_value(path, Options, ?REPO_PATH),
	case filelib:is_dir(RepoDir) of
	true ->
		Command = ?GIT ++ " upload-pack " ++ RepoDir,
		{noreply, data, #state{cmd = Command}};
	false ->
		{error, no_port}
	end.

data(Message, State) ->
	io:format("~p~n", [Message]),
	{noreply, terminate, State}.
	
terminate(_, _, _) ->
	ok.

%% stub for now
encode(Bin) when is_binary(Bin) ->
	{ok, Bin}.
decode(Bin) ->
	{ok, Bin}.


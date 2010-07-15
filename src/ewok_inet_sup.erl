%% Copyright 2009-2010 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(ewok_inet_sup).
-name("Ewok Network Service Supervisor").
-depends([kernel, stdlib]).

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_service).
-export([start_link/1, stop/0]).
-export([start_services/1]).
-behaviour(supervisor).
-export([init/1]).

%%
start_link(Opts) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, [Opts]).
%%
stop() ->
	%% supervisor:terminate_child... all?
	ok.
	
%% supervisor callback
init([]) ->
	ok;
init([Services|_]) ->
	ewok_log:message(?MODULE, Services),
	ChildSpecs = [childspec(Service, Port) || {Service, Port} <- Services],
	case supervisor:check_childspecs(ChildSpecs) of
	ok -> 
		{ok, {{one_for_one, 0, 1}, ChildSpecs}};
	Error -> 
		Error
	end.
	
%%
childspec(Module, Port) when is_atom(Module), is_integer(Port) ->
	Inet = Module:start(Port),
	Name = ewok_inet:make_name(Port),
%	?TTY(Inet#ewok_inet{id = Name}),
	{Name, {ewok_inet, start_link, [Inet#ewok_inet{id = Name}]}, permanent, 5000, worker, [ewok_inet]}.

%%
start_services([Child|Rest]) ->
	ewok_log:message(?MODULE, [{starting, Child}]),
	case supervisor:start_child(?MODULE, Child) of
	{ok, _Pid} -> 
		start_services(Rest);
	{ok, _Pid, _Info} -> 
		start_services(Rest);
	Error = {error, _} -> 
		Error
	end;
start_services([]) ->
	ok.

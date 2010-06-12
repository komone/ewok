%% Copyright 2010 Steve Davis <steve@simulacity.com>
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
-module(ewok_workflow_sup).
-name("Ewok Workflow Supervisor").

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(supervisor).
-export([init/1]).

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

-export([create/1]).

%%
start_link([]) ->
	ewok_log:message(service, ?MODULE),
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
	%% supervisor:terminate_child... all?
	ok.

%%
init([]) -> 
	{ok, {{one_for_one, 10, 10}, []}}.
	
%%
create(Workflow) ->
	ChildSpec = {Workflow#workflow.id, 
		{ewok_workflow, start_link, [Workflow]}, 
		transient, 5000, worker, [ewok_workflow]},
	supervisor:start_child(?MODULE, ChildSpec).

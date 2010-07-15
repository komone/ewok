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

-module(ewok_sup).
-name("Ewok AS Supervisor").
-depends([kernel, stdlib]).

-include("ewok.hrl").
-include("ewok_system.hrl").

-behaviour(supervisor).
-export([init/1]).

%% TODO: ewok_sup is actually a 'fake' service
-export([start_services/1, upgrade/0]).

%% supervisor callback
init(Args) ->
	ChildSpecs = [childspec(Service) || Service <- Args],
	case supervisor:check_childspecs(ChildSpecs) of
	ok -> 
		{ok, {{one_for_one, 0, 1}, ChildSpecs}};
	Error -> 
		Error
	end.

%%
start_services([Child|Rest]) ->
	ewok_log:message(?MODULE, [{starting, Child}]),
	ChildSpec = childspec(Child),
	case supervisor:start_child(?MODULE, ChildSpec) of
	{ok, _Pid} -> 
		start_services(Rest);
	{ok, _Pid, _Info} -> 
		start_services(Rest);
	Error = {error, _} -> 
		Error
	end;	
start_services([]) ->
	ok.
	
%% TODO: Review these options on a per-service basis after
%% performance tests. For now, just set reasonable defaults.
%%
childspec(Module) when is_atom(Module) ->
	% code:ensure_loaded(Other), erlang:function_exported(Other, start_link, 0)..,?
	{Module, {Module, start_link, [[]]}, 
		permanent, 5000, worker, [Module]};
%%
childspec({Module, Args}) when is_atom(Module), is_list(Args) ->
	{Module, {Module, start_link, [Args]}, 
		permanent, infinity, supervisor, [Module]}. %% double check this!
	
%% TODO: what to do here? appup?
upgrade() -> 
	not_implemented.

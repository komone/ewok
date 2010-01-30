%% Copyright 2009 Steve Davis <steve@simulacity.com>
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

-module(ewok_sup).
-vsn("1.0.0").
-name("Ewok AS Supervisor").
-depends([kernel, stdlib]).

-include("ewok.hrl").

-behaviour(supervisor).
-export([init/1]).

%% TODO: ewok_sup is actually a 'fake' service...
%% TODO: remove "upgrade" and figure out how to do it better
-export([start_services/1, upgrade/0]).

%%
%% supervisor callback
%%
init(Args) ->
	ChildSpecs = [spec(Service) || Service <- Args],
	case supervisor:check_childspecs(ChildSpecs) of
	ok -> {ok, {{one_for_all, 0, 1}, ChildSpecs}};
	Error -> Error
	end.

%%
start_services([Child|Rest]) ->
	ewok_log:message(?MODULE, [{starting, Child}]),
	case supervisor:start_child(?MODULE, Child) of
	Error = {error, _} -> Error;
%	{ok, undefined} -> {error, {service_failure, Child}};
	_ -> start_services(Rest)
	end;	
start_services([]) ->
	ok.
	
%% TODO: Review all of these options on a per-service basis after
%% performance tests. For now, just set reasonable defaults.

%% @deprecated
spec(ewok_cache_srv) -> 
	{ewok_cache_srv, {ewok_cache_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_cache_srv]};
%%
spec(ewok_data_srv) -> 
	{ewok_data_srv, {ewok_data_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_data_srv]};
spec(ewok_identity_srv) -> 
	{ewok_identity_srv, {ewok_identity_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_identity_srv]};
spec(ewok_scheduler_srv) -> 
	{ewok_scheduler_srv, {ewok_scheduler_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_scheduler_srv]};
spec(ewok_deployment_srv) -> 
	{ewok_deployment_srv, {ewok_deployment_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_deployment_srv]};
spec(ewok_session_srv) -> 
	{ewok_session_srv, {ewok_session_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_session_srv]};
spec(ewok_workflow_sup) ->
	{ewok_workflow_sup, {ewok_workflow_sup, start_link, []}, 
		permanent, infinity, supervisor, [ewok_workflow_sup]};
spec(ewok_http_srv) -> 
	{ewok_http_srv, {ewok_http_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_http_srv]};
%% review (non-existent)
spec(ewok_geoip_srv) -> 
	{ewok_geoip_srv, {ewok_geiop_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_geiop_srv]};
spec(Other) when is_atom(Other) ->
	% code:ensure_loaded(Other), erlang:function_exported(Other, start_link, 0)..,?
	{Other, {Other, start_link, []}, 
		permanent, 5000, worker, [Other]}.

%% what to do here?
upgrade() -> 
	ok.

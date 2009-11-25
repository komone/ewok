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
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl").

-behaviour(supervisor).
-export([init/1]).

%% TODO: ewok_sup is actually a 'fake' service...
%% TODO: remove "upgrade" and figure out how to do it better
-export([service_info/0, start_services/1, upgrade/0]).

%% NOTE: This is a 'fake' service
service_info() -> [ 
	{name, "Ewok AS Supervisor"}, 
	{version, {1,0,0}}, 
	{depends, [kernel, stdlib]} 
].


%%
%% supervisor callback
%%
init(Args) ->
    {ok, _IP} = %% IMPL: Maybe use this later -- for now ignore
		case os:getenv("EWOK_IP") of 
		false -> {ok, {0,0,0,0}};
		Any -> inet_parse:address(Any) 
		end,
	ChildSpecs = [spec(Service) || Service <- Args],
	case supervisor:check_childspecs(ChildSpecs) of
	ok ->
		%% IMPL: Start the cache server first as other services
		%% depend on configuration parameters being available. 
		%% Services are thus started after init has completed
		{ok, {{one_for_one, 10, 10}, [spec(ewok_cache_srv)]}};
	Error ->
		Error
	end.

%%
start_services(Args) ->
	ChildSpecs = [spec(Service) || X = Service <- Args, X =/= ewok_cache_srv],
	start_services1(ChildSpecs).

start_services1([Child|Rest]) ->
	case supervisor:start_child(?MODULE, Child) of
	Error = {error, _} -> Error;
%	{ok, undefined} -> {error, {service_failure, Child}};
	_ -> start_services1(Rest)
	end;	
start_services1([]) ->
	ok.
	
%% TODO: Review all of these options on a per-service basis after
%% performance tests. For now, just set reasonable defaults.
spec(ewok_cache_srv) -> 
	{ewok_cache_srv, {ewok_cache_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_cache_srv]};
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
spec(ewok_geoip_srv) -> 
	{ewok_geoip_srv, {ewok_geiop_srv, start_link, []}, 
		permanent, 5000, worker, [ewok_geiop_srv]};
spec(Other) when is_atom(Other) ->
	% code:ensure_loaded(Other), erlang:function_exported(Other, start_link, 0)..,?
	{Other, {Other, start_link, []}, 
		permanent, 5000, worker, [Other]}.


%% NOT USED (yet?)
%% from mochiweb
upgrade() ->
    {ok, {_, Specs}} = init([]),
    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

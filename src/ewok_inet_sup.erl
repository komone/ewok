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
start_link([]) ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).
%%
stop() ->
	%% supervisor:terminate_child... all?
	ok.
	
%% supervisor callback
init(Args) ->
	ewok_log:message(?MODULE, Args),
	ChildSpecs = [childspec(Service) || Service <- Args],
	{ok, {{one_for_one, 0, 1}, ChildSpecs}}.
	
%%
childspec(Conf = #ewok_inet{name=Name}) ->
	{Name, {ewok_inet, start_link, [Conf]}, permanent, 5000, worker, [ewok_inet]}.

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

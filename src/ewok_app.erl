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

-module(ewok_app).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl").

-behaviour(application).
-export([start/2, config_change/3, prep_stop/1, stop/1]).

-define(SUPERVISOR, ewok_sup).

%% 
start(Type, Args) ->	
	ewok_logging_srv:start_link(),
	%% ensure boot logging is available
	case lists:member(ewok_logging_srv, gen_event:which_handlers(error_logger)) of
	true -> 
		%% IMPL: note that at this time, the boot log is the default log,
		%% we do not want any log_level filtering so we use the log call direct
		%% throughout bootstrap.
		ewok_log:log(default, server, ewok:ident()),
		{ok, Host} = inet:gethostname(),
		{ok, HostEnt} = inet:gethostbyname(Host),
		ewok_log:log(default, server, [{node, node()}, HostEnt]),
		ewok_log:log(default, loaded, application:loaded_applications()),
		
		Success = start_ewok(Type, Args),
		ewok_log:log(default, bootstrap, Success),
		Success;
	_ -> 
%		io:format(user, "FATAL: Boot logger failed to initialize.", []),
		{error, boot_log_init}
	end.

%
config_change(_Changed, _New, _Removed) ->  
	ok.
%
prep_stop(State) -> 
	State.
%% 
stop(_State) ->
	ok.

%%
start_ewok(_Type, Args) ->
	case supervisor:start_link({local, ?SUPERVISOR}, ?SUPERVISOR, Args) of
	{ok, Pid} ->
		%% IMPL: By calling ewok:configure here, we avoid the need for each 
		%% service to double-check that ewok is properly configured.
		{ok, _NumKeys} = ewok:configure(),
		case ewok_sup:start_services(Args) of
		ok ->
			ewok_log:init_server_log(),
			ewok:autodeploy(),
			{ok, Pid};
		Error -> Error
		end;
	Other -> Other
	end.

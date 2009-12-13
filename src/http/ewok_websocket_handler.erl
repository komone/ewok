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

-module(ewok_websocket_handler).
-vsn("1.0.0").
-author('steve@simulacity.com').

-include("ewok.hrl").

-behaviour(ewok_http_resource).
-export([filter/1, resource_info/0, 'GET'/2]).

resource_info() -> [
	{name, "Ewok Web Socket"}
].

filter(_Request) ->
	ok.

'GET'(Request, _Session) ->
	Upgrade =
		try begin
			<<"Upgrade">> = Request:header(<<"Connection">>),
			<<"WebSocket">> = Request:header(<<"Upgrade">>),
			Request:websocket(),% ANYTHING ELSE?
			ok
		end catch
		Error:Reason ->
			{Error, Reason}
		end,
	case Upgrade of
	ok ->
		Protocol = Request:header(<<"WebSocket-Protocol">>),
		case Protocol of
		%% WebSocket modules to handle custom protocols should be added here...
		% undefined -> ??
		% myprotocol -> myprotocol:start() ??
		_ -> 
			{ok, Pid} = ewok_websocket:start(Request:socket()),
			ok = ewok_socket:controlling_process(Request:socket(), Pid),
			switching_protocols
		end;
	_ -> 
		bad_request	
	end.

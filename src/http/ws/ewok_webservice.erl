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

-module(ewok_webservice).
-include("ewok.hrl").
-include("esp.hrl").

-behavior(ewok_http_resource).
-export([filter/1, resource_info/0]).

-export(['GET'/2, 'POST'/2]).

-compile(export_all).

filter(_Request) ->
	ok.

resource_info() -> [].

'GET'(#http_request{path = Path}, #http_session{data = Data}) ->
	case proplists:get_value(delegated, Data) of
	{?MODULE, [Module]} ->	
		?TTY({encode_wsdl, Module}),
		Wsdl = ewok_wsdl:encode(#wsdl{id = Module, path = Path}),
		?TTY(Wsdl),
		{ok, [{content_type, <<"text/xml">>}], Wsdl};
	Value ->
		?TTY(Value),
		ok
	end.
	
'POST'(_Request, _Session) ->
	ok.

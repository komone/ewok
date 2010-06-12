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

-module(ewok_codec).
-include("ewok.hrl").

-export([encode/2, decode/2]).

-export([behaviour_info/1]).

behaviour_info(callbacks) -> [
	{encode, 1},
	{decode, 1}
];

behaviour_info(_) ->
    undefined.

encode(Codec, Term) -> when is_atom(Codec) ->
	encode(Codec, Term, []).
	
encode(Codec, Term, Acc) ->
	case Codec:encode(Term) of
	{continue, Term1, Acc1} ->
		encode(Codec, Term1, [Acc1|Acc]);
	{ok, Bin} -> 
		list_to_binary(lists:reverse([Bin|Acc])
	end.
	
decode(Codec, Bin) when is_atom(Codec), is_binary(Bin) ->
	

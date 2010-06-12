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

-module(ldap).
-include("ldap.hrl").
-include("ewok.hrl").

-behaviour(ewok_codec).
-export([encode/1, decode/1]).

-export([oid/1, status_code/1]).

-compile(export_all).

-define(BIND_REQUEST, 0).
-define(BIND_RESPONSE, 1).
-define(UNBIND_REQUEST, 2).
-define(SEARCH_REQUEST, 3).
-define(SEARCH_RESPONSE, 4).
-define(MODIFY_REQUEST, 101).
-define(MODIFY_RESPONSE, 102).
-define(ADD_REQUEST, 103).
-define(ADD_RESPONSE, 104).
-define(DEL_REQUEST, 105).
-define(DEL_RESPONSE, 106).
-define(MODIFY_RDN_REQUEST, 107).
-define(MODIFY_RDN_RESPONSE, 108).
-define(COMPARE_REQUEST, 109).
-define(COMPARE_RESPONSE, 110).
-define(ABANDON_REQUEST, 111).

-define(BASE_OBJECT, 0).
-define(SINGLE_LEVEL, 1).
-define(SUBTREE, 2).

%% TODO: (but later) delegate TLV to a BER module

test() ->
%	Bin = <<0,0,0,1,96,22,2,1,3,4,7,99,110,61,116,101,115,116,128,8,112,97,115,115,119,111,114,100>>,
	Bin = <<48, 24, 2,1,1, 96, 19, 2,1,3, 4,4, 101,119,111,107, 128,8, 112,97,115,115,119,111,114,100>>,
	{asn, _, Term} = asn_ber:decode(Bin),
	case Term of
	{sequence, [MessageId, Message = {application, _, _}]} ->
		Op = decode_op(Message),
		#ldap_message{id = MessageId, op = Op, controls = []};
	{sequence, [MessageId, Message, Controls]} ->
		#ldap_message{id = MessageId, op = Message, controls = Controls};
	_ -> 
		{error, Term}
	end.

%%
encode(#ldap_message{id = Id, op = Op, controls = Controls}) ->
	BerOp = encode_op(Op),
	BerControls = encode_controls(Controls),
	case Controls of 
	[] ->
		asn_ber:encode([Id, BerOp]);
	_ ->
		asn_ber:encode([Id, BerOp, BerControls])
	end.

%% TODO
encode_controls(_Controls) ->
	[].

%%
decode(Bin) ->
	[Id, BerOp, BerCtls] = asn_ber:decode(Bin),
	Op = decode_op(BerOp),
	Controls = decode_controls(BerCtls),
	#ldap_message{
		id = Id,
		op = Op,
		controls = Controls
	}.

decode_controls(_Controls) ->
	[].
%%
%% Internal Functions
%%

%%
encode_op(#bind_request{version = Version, name = Name, auth = Auth}) ->
	{application, ?BIND_REQUEST, [Version, Name, Auth]};
%
encode_op(#unbind_request{}) ->
	{application, ?UNBIND_REQUEST, null};
% record(search_request, {base_object, scope, deref = never_deref, max_size = 0, timeout = infinity, types_only = false, filter, attributes}).
encode_op(_R = #search_request{}) ->
	{application, ?SEARCH_REQUEST, []}. %% hmmm

%%
decode_op({application, ?BIND_REQUEST, [Version, Name, Auth]}) when is_binary(Auth) ->
	#bind_request{version = Version, name = Name, auth = Auth};	
%
decode_op({application, ?BIND_RESPONSE, [Code, DN, Message]}) ->
	#ldap_result{code = Code, matched_dn = DN, diagnostic = Message};
%
decode_op({application, ?UNBIND_REQUEST, null}) ->
	#unbind_request{}.

%%	
oid(Bin) ->
	BinList = ewok_text:split(Bin, <<"\\.">>, [{return, list}]),
	IntList = [list_to_integer(X) || X <- BinList],
	list_to_tuple(IntList).

%% LDAP STATUS/ERROR CODES
status_code(success)                      -> 0;
status_code(operations_error)             -> 1;
status_code(protocol_error)               -> 2;
status_code(time_limit_exceeded)          -> 3;
status_code(size_limit_exceeded)          -> 4;
status_code(compare_false)                -> 5;
status_code(compare_true)                 -> 6;
status_code(auth_method_not_supported)    -> 7;
status_code(stronger_auth_required)       -> 8;
%-- 9 reserved --
status_code(referral)                     -> 10;
status_code(admin_limit_exceeded)         -> 11;
status_code(unavailable_critical_extension) -> 12;
status_code(confidentiality_required)     -> 13;
status_code(sasl_bind_in_progress)        -> 14;
%-- 15 unused -- 
status_code(no_such_attribute)            -> 16;
status_code(undefined_attribute_type)     -> 17;
status_code(inappropriate_matching)       -> 18;
status_code(constraint_violation)         -> 19;
status_code(attribute_or_value_exists)    -> 20;
status_code(invalid_attribute_syntax)     -> 21;
%-- 22-31 unused --
status_code(no_such_object)               -> 32;
status_code(alias_problem)                -> 33;
status_code(invalid_dn_syntax)            -> 34;
%-- 35 reserved for undefined isLeaf --
% status_code(is_leaf)                      -> 35;
status_code(alias_dereferencing_problem)  -> 36;
%-- 37-47 unused --
status_code(inappropriate_authentication) -> 48;
status_code(invalid_credentials)          -> 49;
status_code(insufficient_access_rights)   -> 50;
status_code(busy)                         -> 51;
status_code(unavailable)                  -> 52;
status_code(unwilling_to_perform)         -> 53;
status_code(loop_detect)                  -> 54;
%-- 55-63 unused --
status_code(naming_violation)             -> 64;
status_code(object_class_violation)       -> 65;
status_code(not_allowed_on_non_leaf)      -> 66;
status_code(not_allowed_on_rdn)           -> 67;
status_code(entry_already_exists)         -> 68;
status_code(object_class_mods_prohibited) -> 69;
%-- 70 reserved for CLDAP --
status_code(affects_multiple_dsas)        -> 71;
%-- 72-79 unused --
status_code(other)                        -> 80.

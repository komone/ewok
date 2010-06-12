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

%% @def RFCs: 4510,4511,4512,4513,4514,4515,4516,4517,4518,4519

%% IANA-assigned LDAP ports
-define(LDAP_VERSION, 3).
-define(LDAP_PORT, 389).
-define(LDAPS_PORT, 636).

-define(MAX_INT, 2147483647). % 32-bit positive integer

-record(ldap_message, {id, op, controls = []}).
-record(ldap_result, {code, matched_dn, diagnostic}).

-record(control, {type, criticality = false, value}).

-record(bind_request, {version = ?LDAP_VERSION, name, auth}).
-record(unbind_request, {}).
-record(search_request, {base, scope, deref = never_deref, max_size = 0, timeout = infinity, types_only = false, filter, attributes}).
-record(modify_request, {object, changes = []}).
-record(add_request, {entry, attributes = []}).
-record(del_request, {entry, attributes = []}).
-record(modify_dn_request, {entry, new_rdn, delete_old_rdn = true, new_superior}).
-record(compare_request, {entry, attr_value_assert}).
-record(abandon_request, {}).
-record(extended_request, {name, value}).

-record(sasl, {mechanism, credentials}).


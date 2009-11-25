
%%
%% IANA-assigned LDAP port
-define(LDAP_PORT, 389).

-record(ldap_message, {seq, op, controls=[]}).
-record(ldap_result, {code, matched_dn, diagnostic_message}).

-record(control, {type, criticality=false, value}).

-record(add_request, {entry, attributes=[]}).
-record(bind_request, {version, name, auth_choice=simple}).
-record(compare_request, {entry, attr_value_assert}).
-record(extended_request, {name, value}).
-record(modify_request, { object, changes=[]}).
-record(modify_dn_request, {entry, new_rdn, delete_old_rdn=true, new_superior}).
-record(search_request, {scope=base_object, deref=never_deref, max_size=0, timeout=infinity, types_only=false, filter, attributes}).

-record(sasl, {mechanism, credentials=""}).


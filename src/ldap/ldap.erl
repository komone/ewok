%%
-module(ldap).
-export([code/1]).

code(success)                      -> 0;
code(operations_error)             -> 1;
code(protocol_error)               -> 2;
code(time_limit_exceeded)          -> 3;
code(size_limit_exceeded)          -> 4;
code(compare_false)                -> 5;
code(compare_true)                 -> 6;
code(auth_method_not_supported)    -> 7;
code(stronger_auth_required)       -> 8;
%-- 9 reserved --
code(referral)                     -> 10;
code(admin_limit_exceeded)         -> 11;
code(unavailable_critical_extension) -> 12;
code(confidentiality_required)     -> 13;
code(sasl_bind_in_progress)        -> 14;
%-- 15 unused -- 
code(no_such_attribute)            -> 16;
code(undefined_attribute_type)     -> 17;
code(inappropriate_matching)       -> 18;
code(constraint_violation)         -> 19;
code(attribute_or_value_exists)    -> 20;
code(invalid_attribute_syntax)     -> 21;
%-- 22-31 unused --
code(no_such_object)               -> 32;
code(alias_problem)                -> 33;
code(invalid_dn_syntax)            -> 34;
%-- 35 reserved for undefined isLeaf --
code(is_leaf)                      -> undefined;
code(alias_dereferencing_problem)  -> 36;
%-- 37-47 unused --
code(inappropriate_authentication) -> 48;
code(invalid_credentials)          -> 49;
code(insufficient_access_rights)   -> 50;
code(busy)                         -> 51;
code(unavailable)                  -> 52;
code(unwilling_to_perform)         -> 53;
code(loop_detect)                  -> 54;
%-- 55-63 unused --
code(naming_violation)             -> 64;
code(object_class_violation)       -> 65;
code(not_allowed_on_non_leaf)      -> 66;
code(not_allowed_on_rdn)           -> 67;
code(entry_already_exists)         -> 68;
code(object_class_mods_prohibited) -> 69;
%-- 70 reserved for CLDAP --
code(affectsMultipleDSAs)          -> 71;
%-- 72-79 unused --
code(other)                        -> 80;
code(_)                            -> undefined.

% -*- mode: erlang -*-

%% Compile behaviour definitions first
{"src/ewok_service.erl", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.
{"src/datasource/ewok_datasource.erl", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.
{"src/http/ewok_http_resource.erl", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.
{"src/http/ewok_web_application.erl", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.

%% Then everything else 
{"src/*", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.
{"src/*/*", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.
%{"src/datasource/*", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.
%{"src/http/*", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.
%{"src/smtp/*", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.
%{"src/ldap/*", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.
%{"src/utp/*", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.

%% Then built-in applications
{"priv/apps/beep/src/*", [{i, "include"}, {outdir, "priv/apps/beep/ebin"}, debug_info, strict_record_tests]}.
{"priv/apps/admin/src/*", [{i, "include"}, {outdir, "priv/apps/admin/ebin"}, debug_info, strict_record_tests]}.
{"priv/apps/redoc/src/*", [{i, "include"}, {outdir, "priv/apps/redoc/ebin"}, debug_info, strict_record_tests]}.
{"priv/apps/wiki-1.0.0/src/*", [{i, "include"}, {outdir, "priv/apps/wiki-1.0.0/ebin"}, debug_info, strict_record_tests]}.

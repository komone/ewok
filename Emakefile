% -*- mode: erlang -*-

%% Compile behaviour definitions first
{"src/ewok_service.erl", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.
{"src/ewok_datasource.erl", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.
{"src/ewok_http_resource.erl", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.

%% Then everything else 
{"src/*", [{i, "include"}, {outdir, "ebin"}, debug_info, strict_record_tests]}.

%% Then built-in applications
{"priv/apps/admin/src/*", [{i, "include"}, {outdir, "priv/apps/admin/ebin"}, debug_info, strict_record_tests]}.
{"priv/apps/redoc/src/*", [{i, "include"}, {outdir, "priv/apps/redoc/ebin"}, debug_info, strict_record_tests]}.
{"priv/apps/wiki/src/*", [{i, "include"}, {outdir, "priv/apps/wiki/ebin"}, debug_info, strict_record_tests]}.

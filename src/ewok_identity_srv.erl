-module(ewok_identity_srv).
-vsn("1.0").
-author('steve@simulacity.com').
-include("ewok.hrl").

-behavior(ewok_service).
-export([start_link/0, stop/0, service_info/0]).

-compile(export_all).

-define(KEYSTORE, ".keystore").

start_link() ->
	ewok_log:log(default, service, {?MODULE, service_info()}),
	crypto:start(),
	{ok, []}.
	
stop() ->
	ok.

service_info() -> [
	{name, "Ewok Identity Service"},
	{version, {1,0,0}},
	{comment, ""}
].

keystore() ->
	Path = ewok_config:get("ewok.identity.keystore", "./priv/data"),
	Dir = filename:join(ewok_util:appdir(), Path),
	case filelib:is_dir(Dir) of
	true ->
		File = filename:join(Dir, ?KEYSTORE),
		case filelib:is_regular(File) of
		true ->	
			{ok, [Term]} = file:consult(File),
			Term;
		false ->
			{error, no_keystore}
		end;
	false ->
		{error, invalid_path}
	end.

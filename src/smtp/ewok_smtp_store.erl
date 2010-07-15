-module(ewok_smtp_store).
-include("ewok.hrl").
-export([create/0, save/1]).

create() ->
	ok.
	
save(Record) ->
	?TTY({mail, Record}).
	

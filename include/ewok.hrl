%% File: ewok.hrl
%% Version: 1.0.0 beta
%% Author: Steve Davis <steve@simulacity.com>
%% Updated: January 24, 2010
%% Description: Definitions and records that may be used in applications
-define(SERVER_ID, <<"Ewok/1.0 BETA (Wicket)">>).

-define(CONFIG_FILE_EXT, <<".config">>). % unused
-define(ARCHIVE_FILE_EXT, <<".ez">>).

-define(ESP_FILE_EXT, <<".esp">>).
-define(LOG_FILE_EXT, <<".log">>).
-define(LOG_ARCHIVE_EXT, <<".tar.gz">>).

%% Debugging use only. Remove all usage instances at release.
%% Do not use io:format directly in the source code as this macro 
%% will make finding and removing spurious io: messages harder.
%% Commenting out this macro will allow us to find
%% all development console messages in the code very easily.
-define(TTY(Term), io:format(user, "[DEBUG] ~p~n", [Term])).

%% Only semi-valid. There just *has* to be a better way :(
-define(is_string(S), (is_list(S) andalso S =/= [] andalso is_integer(hd(S)))).
%% This is another semi-valid guard. NOTE: used instead of is_record/2 when runtime type isn't known.
-define(is_record(X), is_tuple(X) andalso is_atom(element(1, X)) andalso size(X) > 1).

%% This record is not (currently) used; It is more a statement of intent and a
%% possible semantic approach to "strings" that more closely observes genuine 
%% engineering principles and standards. You can safely ignore this record for now.
-record(text, {bin= <<>>, charset=utf8, lang='us-en'}).

%% Ewok Session Management
-define(EWOK_SESSION_KEY, <<"_EWOKSID">>).
-record(session, {key, ip, user, data=[], started, expires, ttl, notify}).

%% end %%

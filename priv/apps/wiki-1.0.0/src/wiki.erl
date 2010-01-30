%%
-module(wiki).
-vsn("1.0").
-include_lib("ewok/include/ewok.hrl").
-include_lib("ewok/include/esp.hrl").

-export([find/1, save/3, links/0]).

-export([init_db/0]). %% how to deal with new app tables?

-record(ewok_wiki, {path, title, content}).

%%
init_db() ->
	ewok_db:create_tables([{ewok_wiki, record_info(fields, ewok_wiki)}]),
    ok = mnesia:wait_for_tables([ewok_wiki], 10000).

%%
find(Path) ->
	case ewok_db:read(ewok_wiki, Path) of
	#ewok_wiki{title=Title, content=Content} -> 
		[{title, Title}, {content, [#h1{body=[Title]}, Content]}];
	undefined -> 
		undefined
	end.

%%
save(Path, Title, Content) ->
	case ewok_db:lookup(ewok_wiki, Path) of
	#ewok_wiki{} = Wiki ->
		ewok_db:update(Wiki#ewok_wiki{title=Title, content=Content});
	undefined ->
		ewok_db:create(#ewok_wiki{path=Path, title=Title, content=Content})
	end.

%%
links() ->
	{ok, Pages} = ewok_db:select(ewok_wiki),
	[#a{href = Path, body = [Title]} || #ewok_wiki{path = Path, title = Title} <- Pages].

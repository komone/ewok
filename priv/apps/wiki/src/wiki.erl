%%
-module(wiki).
-vsn("1.0").
-include("ewok.hrl").
-include("esp.hrl").

-compile(export_all).

-record(wiki, {path, route, content}).

init_db() ->
	mnesia:start(),
	ewok_db:create_tables([
		{wiki, record_info(fields, wiki)}
	]),
    ok = mnesia:wait_for_tables([wiki], 10000).

%%
get_routes() -> 
	{ok, Wiki} = ewok_db:select(wiki),
	[W#wiki.route || W <- Wiki].

%%
create(Route, Content) when is_record(Route, route), is_list(Content) ->
	case ewok_config:lookup(Route#route.path) of
	{error, _} -> create_page(Route, Content);
	#route{path=default} -> create_page(Route, Content);
	#route{} -> {error, exists}
	end.
%%
create_page(Route, Content) ->
	ewok_db:create(#wiki{path=Route#route.path, route=Route, content=Content}).

%%
read(Path) ->
	case ewok_db:read(wiki, Path) of
	[] -> [];
	Wiki -> Wiki#wiki.content
	end.

%%
update(Path, Content) ->
	Wiki = ewok_db:read(wiki, Path),
	ewok_db:update(Wiki#wiki{content=Content}).

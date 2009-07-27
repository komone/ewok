-module(wiki_web).

-vsn("1.0").
-author('steve@simulacity.com').

-include("ewok.hrl").
-include("esp.hrl").

-behavior(ewok_http_resource).
-export([filter/1, do/3, resource_info/0]).

%%
%% Resource Callbacks
%%
resource_info() -> [{name, "Wiki Handler"}].

%%
filter(_Request) ->  
	ok.
%%
do('GET', Request, Session) ->
	Spec = case ewok_wiki:read(Request:get_path()) of
		[] -> [ {title, <<"Wiki - Untitled">>},
				{menu, [#p{body=[<<"No Pages">>]}]},
				{content, [
					#form{method=post, action=Request:get_path(), body=[
						#grid{body=[
							[<<"Path: ">>, #input{type=text, name="path"}],
							[<<"Title: ">>, #input{type=text, name="title"}],
							[<<"Content: ">>, #textarea{name="content"}],
							[<<>>, #input{type=submit}]
						]}
					]}
				]}];
		Content -> Content
		end,
	ewok_admin:page(Spec, Request, Session);
%%
do('POST', Request, _Session) ->
	Path = Request:parameter("path"),
	Title = Request:parameter("title"),
	Content = Request:parameter("content"),
	Route = #route{path=Path, handler=ewok_wiki_handler, realm=any, roles=any},
	Spec = [{title, Title}, 
		{menu, menu()},
		{content, [Content]}
	],
	try begin
		ok = wiki:create_page(Route, Spec),
		ok = ewok_cache_srv:add(Route),
		{found, [{location, ewok_http:absolute_uri(Path)}], []}
	end catch
	_:_ -> internal_server_error
	end;
%
do(_, _, _) ->
	not_implemented.

menu() ->
	[].

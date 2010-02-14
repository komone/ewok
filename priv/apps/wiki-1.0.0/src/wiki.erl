-module(wiki).
-name("Ewok Wiki").

-include_lib("ewok/include/ewok.hrl").
-include_lib("ewok/include/esp.hrl").

-behaviour(ewok_http_resource).
-export([resource_info/0, filter/1, 'GET'/2, 'POST'/2]).

-record(ewok_wiki, {path, title, content}).

%%
%% Resource Callbacks
%%
resource_info() -> [
	{ewok_db, [{ewok_wiki, record_info(fields, ewok_wiki)}]}
].

%%
filter(_Request) ->  
	ok.
	
%%
'GET'(Request, Session) ->
	Path = Request:path(),
%	?TTY({wiki_path, Path}),
	case find_page(Path) of
	undefined ->
		Page = edit_page(Session, Path);
	Content -> 
		Page = [{dock, ewok_web:dock(Session)}, {menu, menu()}|Content]
	end,
	render(ok, Page).

%%
'POST'(Request, _Session) ->
	Path = Request:path(),
	Title = Request:parameter(<<"title">>),
	Content = Request:parameter(<<"content">>),
%	?TTY({Title, Content}), 
	try esp_validator:not_null([Title, Content]) of
	true ->
		ok = save_page(Path, Title, Content),
		{found, [{location, Path}], []};
	_ ->		
		{found, [{location, Path}], []}
	catch
	E:R -> 
		?TTY({wiki_post, {E, R}}), 
		internal_server_error
	end.

%%
find_page(Path) ->
	case ewok_db:read(ewok_wiki, Path) of
	#ewok_wiki{title=Title, content=Content} -> 
		[{title, Title}, {content, [#h1{body=[Title]}, Content]}];
	undefined -> 
		undefined
	end.

%%
save_page(Path, Title, Content) ->
	case ewok_db:lookup(ewok_wiki, Path) of
	#ewok_wiki{} = Wiki ->
		ewok_db:update(Wiki#ewok_wiki{title = Title, content = Content});
	undefined ->
		ewok_db:create(#ewok_wiki{path = Path, title = Title, content = Content})
	end.
%%
menu() ->
	{ok, Pages} = ewok_db:select(ewok_wiki),
	List = [{Path, Title} || #ewok_wiki{path = Path, title = Title} <- Pages],
	Links = [#li{body = [#a{href = Path, body = [Title]}]} || {Path, Title} <- List],
	[#p{body = [<<"WIKI PAGES">>]}, #ul{body = Links}].

%%
edit_page(Session, Path) -> 
	[{title, <<"Wiki - Untitled">>},
	{menu, menu()},
	{dock, ewok_web:dock(Session)},
	{content, [
		#form{method = post, action = Path, body = [
			#grid{body = [
				[<<"Title: ">>, #input{type = text, name = "title"}],
				[<<"Content: ">>, #textarea{name = "content"}],
				[<<>>, #input{type = submit}]
			]}
		]}
	]}].

%%
render(Status, Page) ->
	Title = proplists:get_value(title, Page, <<"Ewok Wiki">>),
	Dock = proplists:get_value(dock, Page, []),
	Head = [
		#link{rel="icon", href="/favicon.png", type="image/png"},
		#css{src="/css/ui-smoothness-1.7.2.css"},
		#css{src="/esp.css"},
		#css{src="/default.css"},
		#script{src="/js/jquery-1.3.2.min.js"},
		#script{src="/js/jquery-ui-1.7.2.min.js"},
		#script{src="/esp-1.0.0.js"},
		proplists:get_value(head, Page, [])
	],
	Body = [
		#'div'{id="top", body=[
			#img{id="logo", src="/images/ewok-logo.png"},
			#'div'{id="dock", body=Dock}
		]},
		#'div'{id="page", body=[
			#'div'{id="nav", body=proplists:get_value(menu, Page, [])},
			#'div'{id="content", body=proplists:get_value(content, Page, [])}
		]},
		#'div'{id="footer", body=[
			#hr{},
			#p{body=[<<"Copyright &copy; 2010 Simulacity.com. All Rights Reserved.">>]}
		]}
	],
	esp:render(Status, #page{title=Title, head=Head, body=Body}).
	

-module(wiki_web).
-name("Ewok Wiki").

-include_lib("ewok/include/ewok.hrl").
-include_lib("ewok/include/ewok_system.hrl").
-include_lib("ewok/include/esp.hrl").

-behaviour(ewok_http_resource).
-export([filter/1, 'GET'/2, 'POST'/2, resource_info/0]).
-compile(export_all).

%%
%% Resource Callbacks
%%
resource_info() -> [].
%%
filter(_Request) ->  
	ok.
%%
'GET'(Request, Session) ->
	Path = Request:path(),
	?TTY({wiki, Path}),
	case wiki:find(Path) of
	undefined ->
		Page = form(Session, Path);
	Content -> 
		Page = [{dock, ewok_web:dock(Session)}, {menu, menu()}|Content]
	end,
	render(ok, Page).

%%
'POST'(Request, _Session) ->
	Path = Request:path(),
	Title = Request:parameter(<<"title">>),
	Content = Request:parameter(<<"content">>),
	?TTY({Title, Content}), 
	try esp_validator:not_null([Title, Content]) of
	ok ->
		ok = wiki:save(Path, Title, Content),
		{found, [{location, Path}], []};
	_ ->		
		{found, [{location, Path}], []}
	catch
	E:R -> 
		?TTY({E, R}), 
		internal_server_error
	end.

menu() ->
	Links = [#li{body=[Link]} || Link <- wiki:links()],
	[#p{body=[<<"WIKI PAGES">>]}, #ul{body=Links}].

form(Session, Path) -> [
	{title, <<"Wiki - Untitled">>},
	{menu, menu()},
	{dock, ewok_web:dock(Session)},
	{content, [
		#form{method=post, action=Path, body=[
			#grid{body=[
				[<<"Title: ">>, #input{type=text, name="title"}],
				[<<"Content: ">>, #textarea{name="content"}],
				[<<>>, #input{type=submit}]
			]}
		]}
	]}
].

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
	%
	esp:render(Status, #page{title=Title, head=Head, body=Body}).
%	#page{title=Title, head=Head, body=Body}.
	

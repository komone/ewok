%%
-module(ewok_web).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl"). 
-include("esp.hrl").
-export([components/0]).
-export([render/3, errorpage/3]).

%%
components() ->
	[title, head, dock, menu, content].

%% try to avoid callbacks like this...
render(Request, Session, Module) when is_atom(Module) ->
	Spec = [
		{title, Module:title(Request, Session)},
		{head, Module:head(Request, Session)},
		{dock, Module:dock(Request, Session)},
		{menu, Module:menu(Request, Session)},
		{content, Module:content(Request, Session)}
	],
	render(Request, Session, Spec);

%%
render(_Request, Session, Spec) ->
	Title = proplists:get_value(title, Spec, <<"Ewok AS">>),
	Dock = proplists:get_value(dock, Spec, dock(Session)),
	Head = [
		#link{rel="icon", href="/favicon.png", type="image/png"},
		#css{src="/css/ui-smoothness-1.7.2.css"},
		#css{src="/esp.css"},
		#css{src="/default.css"},
		#script{src="/js/jquery-1.3.2.min.js"},
		#script{src="/js/jquery-ui-1.7.2.min.js"},
		#script{src="/esp-1.0.0.js"},
		proplists:get_value(head, Spec, [])
	],
	Body = [
		#'div'{id="top", body=[
			#img{id="logo", src="/images/ewok-logo.png"},
			#'div'{id="dock", body=Dock}
		]},
		#'div'{id="page", body=[
			#'div'{id="nav", body=proplists:get_value(menu, Spec, [])},
			#'div'{id="content", body=proplists:get_value(content, Spec, [])}
		]},
		#'div'{id="footer", body=[
			#hr{},
			#p{body=[<<"Copyright &copy; 2009 Simulacity.com. All Rights Reserved.">>]}
		]}
	],
	%
	esp:render(#page{title=Title, head=Head, body=Body}).

%%
dock(Session) ->
	Username = 
		case Session:user() of
		undefined -> 
			#a{href="/login", body=[<<"Log In">>]};
		U = #user{} -> 
			{_, Name} = U#user.name,
			list_to_binary(Name)
		end,
	[#span{class="dock", body=[
		Username, 
		<<" | ">>,
		#a{href="/admin", body=[<<"Dashboard">>]},
		<<" | ">>,
		#a{href="/", body=[<<"News">>]},
		<<" | ">>,
		#a{href="/doc", body=[<<"Documentation">>]},
		<<" | ">>,
		#a{href="/", body=[<<"About">>]}
	]}].

%%
errorpage(Request, Session, Status) when is_atom(Status) ->
	errorpage(Request, Session, ewok_http:status_code(Status));
errorpage(Request, Session, Status) -> 
	Error = [<<"Error ">>, esp_html:text(Status)],
	Spec = [
		{title, Error},
		{dock, []},
		{menu, []},
		{content, [
			#h1{body = Error},
			#p{body = [<<"The following error occured when the browser tried to access this resource:">>]},
			#h3{body = [<<"Status Code ">>, esp_html:text(Status), <<" - ">>, ewok_http:status_message(Status)]}
		]}
	],
	render(Request, Session, Spec).
	

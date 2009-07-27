%%
-module(ewok_web).
-vsn({1,0,0}).
-author('steve@simulacity.com').

-include("ewok.hrl").
-include("esp.hrl").
-export([components/0]).
-export([render/3]).

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
	Head = [
		#css{path="/default.css"},
		#link{rel="icon", href="/favicon.png", type="image/png"},
		proplists:get_value(head, Spec, [])
	],
	Body = [
		#'div'{id="top", body=[
			#img{id="logo", src="/images/ewok-logo.png"},
			#'div'{id="dock", body=dock(Session)}
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
	[#span{class="label", body=[
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


%%
%%
-module(tutorial).

-include_lib("ewok/include/esp.hrl").

-compile(export_all).
-export([date/0]).

%%
init(_Options = []) ->
	{ok, []}.

%% dock, nav, content
page(Dock, Nav, Content) ->
	Title = <<"Tutorial Application">>,
	Head = [
		#css{src="/default.css"},
		#link{rel="icon", href="/favicon.png", type="image/png"}
	],
	Body = [
		#'div'{id="top", body=[
			#img{id="logo", src="/images/beep-logo.png"},
			#'div'{id="dock", body=[Dock]}
		]},
		#'div'{id="page", body=[
			#'div'{id="nav", body=[Nav]},
			#'div'{id="content", body=[Content]}
		]},
		#br{clear="all"},
		#'div'{id="footer", body=[
			#hr{},
			#p{body=[<<"Copyright &copy 2010 Simulacity.com. All Rights Reserved.">>]}
		]}
	],
	esp:render(#page{title=Title, head=Head, body=Body}).

date() ->
	ewok_http:date().
	

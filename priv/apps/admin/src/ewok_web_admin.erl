%%
-module(ewok_web_admin).
-include("ewok.hrl").
-include("esp.hrl").
-include("ewok_system.hrl").

-behaviour(ewok_http_resource).
% http_resource callbacks
-export([filter/1, resource_info/0]).

-export(['GET'/2]).
-compile(export_all).
%%
%% Resource Callbacks
%%
resource_info() -> [{name, "Ewok Web Admin"}].

%%
filter(_Request) ->  
	ok.

%%
'GET'(Request, Session) -> 
	Spec = 
		case Request:pathlist() of
		[<<"admin">>] -> 
			spec(home);
		[<<"admin">>, Page] ->
			spec(binary_to_atom(Page, utf8));
		_ -> 
			spec(not_found)
		end,
%	?TTY("Login ~p~n", [{Request, Session}]),
	ewok_web:render(Request, Session, Spec). 

%%
spec(home) ->
	{ok, Registered} = application:get_key(ewok, registered),
	
	Services = lists:filter(fun(X) -> is_pid(whereis(X)) andalso (X =/= ewok_sup) end, Registered),
	#server{runmode=Runmode, apps=Apps} = ewok_deployment_srv:list(),
	AppNames = [App#web_app.id || App1 = App <- Apps, App1#web_app.deployed =:= true],
	DSNames = [DS#datasource.id || DS1 = DS <- ewok_data_srv:info(), DS1#datasource.valid =:= true],
	[ {title, <<"Ewok AS - Administration">>},
	  {menu, menu()},
	  {content, [
		#h1{body=[<<"Overview">>]},
		#grid{
			caption=[<<"Server Overview">>],
			header=[<<" ">>, <<" ">>],
			body=[
				["Server", ewok:ident()],
				["Runmode", esp_html:text(Runmode)],
				["Applications", esp_html:text(AppNames)],
				["Services", esp_html:text(Services)],
				["Datasources", esp_html:text(DSNames)],
				["Tasks", <<" ">>]
			]
		}
	]}];

%% 
spec(routes) ->
	{ok, Routes} = ewok_db:select(ewok_route),
	Format = fun ({ewok_route, Path, Module, Realm, Access}) -> [ 
		#input{value = esp_html:text(Path)},
		esp_html:text(Module),
		esp_html:text(Realm),
		esp_html:text(Access)
	] end,
	Info = [Format(Param) || Param <- Routes],
%	?TTY("~p~n", [Info]),
	[ {title, <<"Ewok AS - Administration">>},
	  {menu, menu()},
	  {content, [
		#h1{body=[<<"Web Application">>]},
		#grid{
			caption=[<<"Web Application Paths">>],
			header=[<<"Path">>, <<"Module">>, <<"Realm">>, <<"Access">>],
			body=lists:sort(Info)
		}
	  ]}
	];

%% 
spec(configuration) ->
	Config = ewok_config:all(),
	Format = fun ({K, V}) -> [ 
		esp_html:text(K),
		#input{value = esp_html:text(V)}
	] end,
	
	Info = [Format(Param) || Param <- Config],
	[ {title, <<"Ewok AS - Administration">>},
	  {menu, menu()},
	  {content, [
		#h1{body=[<<"Configuration">>]},
		#grid{
			caption=[<<"Configuration Parameters">>],
			header=[<<"Key">>, <<"Value">>],
			body=lists:sort(Info)
		}
	  ]}
	];
%% 
spec(applications) -> 
	#server{apps=Apps} = ewok_deployment_srv:list(),
	%?TTY(Apps),
	F = fun (X) ->
		{Icon, Actions} = 
			case X#web_app.deployed of
			true ->
				{ #img{src="/images/running.gif"}, #a{href="#", body=[<<"undeploy">>]} };
			false ->
				{ #img{src="/images/stopped.gif"}, #a{href="#", body=[<<"deploy">>]} }
			end,
		Module = X#web_app.id,
		{AppName, AppVersion} = 
			case erlang:function_exported(Module, application_info, 0) of
			true -> 
				AppInfo = Module:application_info(),
				Name = proplists:get_value(name, AppInfo, atom_to_list(Module)),
				{Name, proplists:get_value(version, AppInfo, undefined)};
			false -> 
				{atom_to_list(Module), undefined}
			end,
		[ Icon,
		  AppName,
		  esp_html:text(ewok:config({X#web_app.id, realm})),
		  esp_html:text(X#web_app.id),
		  esp_html:text(AppVersion),
		  X#web_app.path,
		  esp_html:text(X#web_app.valid),
		  esp_html:text(X#web_app.deployed),
		  Actions
		] end,
	AppInfo = [F(App) || App <- Apps],

	[ {title, <<"Ewok AS - Administration">>},
	  {menu, menu()},
	  {content, [
		#h1{body=[<<"Applications">>]},
		#grid{
			caption=[<<"Deployment Overview">>],
			header=[
				<<" ">>, <<"Application">>, <<"Realm">>, <<"Module">>, <<"Version">>, 
				<<"Local Path">>, <<"Valid">>, <<"Deployed">>, <<" ">>
			],
			body=AppInfo
		}
	]}];

%%
spec(datasources) -> 
	DS = ewok_data_srv:info(),
	F = fun (X) ->
		{Icon, Actions} = 
			case X#datasource.running of
			yes -> %% TODO: Temp
				{ #img{src="/images/running.gif"}, [
					#a{href="#", body=[<<"stop">>]}
				]};
			true ->
				{ #img{src="/images/running.gif"}, [
					#a{href="#", body=[<<"stop">>]}
				]};
			false ->
				{ #img{src="/images/stopped.gif"}, #a{href="#", body=[<<"start">>]} }
			end,
		[ Icon,
		  #a{
			href="datasources/" ++ esp_html:text(X#datasource.id),
			body=[X#datasource.name]
		  },
		  esp_html:text(X#datasource.id),

		  esp_html:text(X#datasource.mod),
		  esp_html:text(X#datasource.running),
		  esp_html:text(X#datasource.valid),
		  esp_html:text(X#datasource.data),
		  Actions
		] end,
	DSInfo = [F(Db) || Db <- DS],

	[ {title, <<"Ewok AS - Administration">>},
	  {menu, menu()},
	  {content, [
		#h1{body=[<<"Data Sources">>]},
		#grid{
			caption=[<<"Data Source Overview">>],
			header=[<<" ">>, <<"Name">>, <<"ID">>, <<"Type">>, <<"Running">>, <<"Valid">>, <<"Data">>, <<" ">>],
			body=DSInfo
		}
	]}];

%%
spec(services) -> 
	{ok, Services} = application:get_key(ewok, registered),
	F = fun (Service, Delegate) ->
		Pid = whereis(Delegate),
		Running = is_pid(Pid),
		PidText = case is_pid(Pid) of
			true -> 
				[_, Value, _] = re:split(pid_to_list(Pid), "[<>]", [{return, list}]), %% NOTE: Informational only
				Value;
			false ->
				"undefined"
			end,
		{Icon, Actions} = 
			case Running of 
			true ->
				{ #img{src="/images/running.gif"}, [
					#a{href="#", body=[<<"info">>]},
					<<" | ">>,
					#a{href="#", body=[<<"stop">>]} 
				]};
			false ->
				{ #img{src="/images/stopped.gif"}, #a{href="#", body=[<<"start">>]} }
			end,
			
		Props = Service:module_info(attributes),		
		Version = %% TODO: Fix this hack...
			case proplists:get_value('vsn', Props) of
			[X] when is_integer(X) ->
				X;
			X ->
				X
			end,
		
		[ Icon,
		  esp_html:text(proplists:get_value(name, Props, Service)),
		  esp_html:text(Service),
		  esp_html:text(Version),
		  esp_html:text(proplists:get_value(depends, Props, [])),
		  PidText,
		  esp_html:text(is_pid(Pid)),
		  Actions ]
	end,
	ErrorLogger = F(ewok_logging_srv, error_logger),
	Info = [ErrorLogger | [F(X, X) || X <- Services]],
	[ {title, <<"Ewok AS - Administration">>},
	{menu, menu()},
	{content, [
		#h1{body=[<<"Services">>]},
		#grid{
			caption=[<<"Service Availability">>],
			header=[<<" ">>, <<"Service">>, <<"Module">>, <<"Version">>, <<"Depends">>, <<"Pid">>, <<"Running">>, <<" ">>],
			body=Info
		}
	]}];

%%
spec(_) -> [
	{title, <<"Ewok AS - Administration">>},
	{menu, menu()},
	{content, [
		#h1{body=[<<"Not found">>]},
		#p{body=[<<"The requested resource does not exist.">>]},
		#p{body=[#a{href="/admin/home", body=[<<"Admin Home">>]}]}
	]}].

%%
menu() ->[
	#p{body=[<<"ADMINISTRATION">>]},
	#ul{body=[
		#li{body=[#a{href="/admin", body=["Overview"]}]},
		#li{body=[#a{href="/admin/configuration", body=["Configuration"]}]},
		#li{body=[#a{href="/admin/routes", body=["Web Routing"]}]},
		#li{body=[#a{href="/admin/applications", body=["Applications"]}]},
		#li{body=[#a{href="/admin/privileges", body=["Users &amp; Roles"]}]},
		#li{body=[#a{href="/admin/tasks", body=["Tasks"]}]},
		#li{body=[#a{href="/admin/services", body=["Services"]}]},
		#li{body=[#a{href="/admin/datasources", body=["Data Sources"]}]},
		#li{body=[#a{href="/admin/mq", body=["Message Queues"]}]},
		#li{body=[#a{href="/admin/snmp", body=["SNMP"]}]},
		#li{body=[#a{href="/admin/vm", body=["VM"]}]},
		#li{body=[#a{href="/redoc/web_admin", body=["Documentation"]}]}
	]} ].

	

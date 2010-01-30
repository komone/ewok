%%
-module(ewok_users).
-vsn("1.0").
-include("ewok.hrl").
-include("ewok_system.hrl").

-compile(export_all).

%%
login(Domain, Username, Password) ->
	try begin
		Domain1 = list_to_existing_atom(Domain),
		case ewok_db:select(ewok_user, {name, {Domain1, Username}}) of
		{ok, [User]} when is_record(User, ewok_user) ->
			Auth = ewok_db:read(ewok_auth, User#ewok_user.id),
			case Auth#ewok_auth.password of
			Digest when is_binary(Digest) ->
				case crypto:sha(Password) =:= Digest of
				true -> 
					{ok, User};
				false -> 
					{error, invalid_password}
				end;
			undefined ->
				{error, not_activated}
			end;
		{ok, []} ->
			{error, no_user}
		end
	end catch
		error:badarg -> 
			{error, no_domain}
	end.

%%
exists(Realm, Username) ->
	try begin
		Realm1 = list_to_existing_atom(Realm),
		case ewok_db:select(ewok_user, {name, {Realm1, Username}}) of
		{ok, [User]} when is_record(User, ewok_user) ->
			true;
		_ -> false
		end
	end catch
		error:badarg -> 
			{error, no_domain}
	end.

%%
activate(Realm, Username, Activation, Password) when is_list(Realm) ->
	activate(list_to_existing_atom(Realm), Username, Activation, Password);
activate(Realm, Username, Activation, Password) ->
	try begin
		case ewok_db:select(ewok_user, {name, {Realm, Username}}) of
		{ok, [User]} when is_record(User, ewok_user) ->
			case ewok_db:read(ewok_auth, User#ewok_user.id) of
			Auth when is_record(Auth, ewok_auth) ->
				case Auth#ewok_auth.activation of
				Activation ->
					Digest = crypto:sha(Password),
					ewok_db:update(Auth#ewok_auth{password=Digest}),
					{ok, User};
				_ -> 
					{error, invalid_activation}
				end;
			_ ->
				{error, no_user}
			end;
		_ ->
			{error, no_auth}
		end
	end catch
		error:badarg -> 
			{error, no_domain}
	end.

%%
create_user(Realm, Username, Password) when is_list(Realm) ->
	create_user(list_to_existing_atom(Realm), Username, Password);
create_user(Realm, Username, Password) ->
	try begin
		case ewok_db:select(ewok_user, {name, {Realm, Username}}) of
		{ok, []} ->
			ID = ewok_identity:id(),
			ewok_db:create(#ewok_user{id=ID, name={Realm, Username}}),
			Digest = crypto:sha(Password),
			Activation = ewok_identity:key(),
			ewok_db:create(#ewok_auth{id=ID, password=Digest, activation=Activation}),
			{ok, Activation};
		{ok, _} ->
			{error, exists}
		end
	end catch
		error:badarg -> 
			{error, no_domain}
	end.	
%%
create_user(Domain, Username) when is_list(Domain) ->
	create_user(list_to_existing_atom(Domain), Username);
create_user(Domain, Username) ->
	try begin
		case ewok_db:select(ewok_user, {Domain, Username}) of
		{ok, []} ->
			ID = ewok_identity:id(),
			ewok_db:create(#ewok_user{id=ID, name={Domain, Username}}),
			Activation = ewok_identity:key(),
			ewok_db:create(#ewok_auth{id=ID, activation=Activation}),
			{ok, Activation};
		{ok, _} ->
			{error, exists}
		end
	end catch
		error:badarg -> 
			{error, no_domain}
	end.
%%
get_user(ID) ->
	{ok, User} = ewok_db:read(ewok_user, ID),
	User.
%%
get_activation(ID) ->
	{ok, Auth} = ewok_db:read(ewok_auth, ID),
	Auth#ewok_auth.activation.


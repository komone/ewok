%% Copyright 2010 Steve Davis <steve@simulacity.com>
%
% Licensed under the Apache License, Version 2.0 (the "License");
% you may not use this file except in compliance with the License.
% You may obtain a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
% 
% Unless required by applicable law or agreed to in writing, software
% distributed under the License is distributed on an "AS IS" BASIS,
% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
% See the License for the specific language governing permissions and
% limitations under the License.

-module(ewok_sdb).
-name("Ewok SDB Datasource").

-include("ewok.hrl").
-include("ewok_system.hrl").

-export([list_domains/0, list_domains/1, create_domain/1, delete_domain/1,
	get_attributes/2, put_attributes/3, batch_put_attributes/2, 
	delete_attributes/2, delete_attributes/3, run/1, metadata/1]).
-export([select/1]).

-behaviour(ewok_service).
-export([start_link/1, stop/0]).

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, 
	handle_info/2, terminate/2, code_change/3]).

-define(AWS_SDB_HOST, <<"sdb.amazonaws.com">>).
-define(AWS_SDB_VERSION, <<"2007-11-07">>).
-define(KEY_SIZE, 36).
-define(SECURE, false).
-define(SERVER, ?MODULE).

-record(state, {sdb_access_key, sdb_secret_key}).
-record(sdb_request, {action, result = <<>>, params = [], options = [], meta = []}).

%% 
start_link(Args) ->
	ewok_util:check_dependencies([ewok_identity_srv]),
	ssl:start(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Args, []).

%% 
stop() ->
    gen_server:cast(?SERVER, stop).
	
%%
%%% gen_server
%%
init(_Args) ->
	process_flag(trap_exit, true), % when do we need this?
	State = #state{
		sdb_access_key = ewok_identity:keystore(sdb_access),
		sdb_secret_key = ewok_identity:keystore(sdb_secret)
	},
    {ok, State}.
%
handle_call(credentials, _From, State) ->
	{reply, {ok, State#state.sdb_access_key, State#state.sdb_secret_key}, State};
%
handle_call(_Message, _From, State) ->
	{reply, not_implemented, State}.
%%
handle_cast(stop, State) ->
    {stop, normal, State};
%
handle_cast(_, State) ->
    {noreply, State}.
%%
handle_info(_Info, State) ->
    {noreply, State}.
%%
terminate(_Reason, _State) ->
    ok.
%
code_change(_OldVsn, State, _Extra) -> 
	{ok, State}.
	
%% @public
metadata(Domain) when is_binary(Domain) ->
	request_action(#sdb_request{
		action = <<"DomainMetadata">>, 
		params = [{<<"DomainName">>, Domain}],
		meta = [request_id, box_usage]
	}).

%% @public
list_domains() ->
    list_domains([]).
%%
list_domains(Options) when is_list(Options) ->
	request_action(#sdb_request{
		action = <<"ListDomains">>, 
		result = <<"DomainName">>,
		options = Options,		
		meta = [request_id, next_token, box_usage]
	}).

%% @public
%% This function creates a new SimpleDB domain. The domain name must be unique among the 
%% domains associated with your AWS Access Key ID. This function might take 10 
%% or more seconds to complete.. 
create_domain(Domain) when is_binary(Domain) ->
	request_action(#sdb_request{
		action = <<"CreateDomain">>,
		result = <<"DomainName">>,
		params = [{<<"DomainName">>, Domain}],
		meta = [request_id, box_usage]
	}).

%% @public
%% This function deletes a SimpleDB domain. Any items (and their attributes) in the domain 
%% are deleted as well. This function might take 10 or more seconds to complete.
delete_domain(Domain) when is_binary(Domain) ->
	request_action(#sdb_request{
		action = <<"DeleteDomain">>, 
		result = <<"DomainName">>,
		params = [{<<"DomainName">>, Domain}],
		meta = [request_id, box_usage]
	}).
%%
select(Table) ->
	run(<<"select * from ", Table/binary>>).

%% @public
run(Query) when is_binary(Query) ->
	request_action(#sdb_request{
		action = <<"Select">>, 
		result = <<"Item">>,
		params = [{<<"SelectExpression">>, Query}],
		meta = [request_id, box_usage]
	}).

%% @public
%% Returns all of the attributes associated with the items in the given list.
%%
%% If the item does not exist on the replica that was accessed for this 
%% operation, an empty set is returned. The system does not return an 
%% error as it cannot guarantee the item does not exist on other replicas.
get_attributes(Domain, Item) ->
    get_attributes(Domain, Item, [], []).
%% Returns the requested attribute for an item. 
get_attributes(Domain, Item, _Attributes, Options) when is_binary(Domain), is_binary(Item) ->
	Response = request_action(#sdb_request{
		action = <<"GetAttributes">>, 
		result = <<"Attribute">>,
		params = [{<<"DomainName">>, Domain}, {<<"ItemName">>, Item}],
		options = Options,
		meta = [request_id, next_token, box_usage]
	}),
	case Response of
	{ok, Metadata, Results} ->
		Values = [{Key, Value} || [{<<"Name">>, [], [Key]}, {<<"Value">>, [], [Value]}] <- Results],
		{ok, Metadata, lists:sort(Values)};
	_ -> 
		Response
	end.

%% @public
%% This function cannot have two attribute instances where 
%% both the attribute name and value are the same.
%%
%% Optionally, you can supply the Replace parameter for each individual 
%% attribute. Setting this value to true causes the new attribute value 
%% to replace the existing attribute value(s). For example, if an item has 
%% the attributes { "a", ["1"] }, { "b", ["2","3"]} and you call this function 
%% using the attributes { "b", "4", true }, the final attributes of the item 
%% are changed to { "a", ["1"] } and { "b", ["4"] }, which replaces the previous 
%% values of the "b" attribute with the new value.
%%
%% Using this function to replace attribute values that do not exist will not 
%% result in an error.
%%
%% The following limitations are enforced for this operation:
%% - 100 attributes per each call
%% - 256 total attribute name-value pairs per item
%% - 250 million attributes per domain
%% - 10 GB of total user data storage per domain
put_attributes(Domain, Item, Attributes) when is_binary(Domain), 
		is_binary(Item), is_list(Attributes) ->
	AttributeList = make_attribute_parameters(Attributes),
	request_action(#sdb_request{
		action = <<"PutAttributes">>, 
		params = [{<<"DomainName">>, Domain}, {<<"ItemName">>, Item} | AttributeList],
		meta = [request_id, box_usage]
	}).
%%
make_attribute_parameters(Attributes) ->
	make_attribute_parameters(Attributes, 1, []).
%
make_attribute_parameters([{K, V}|T], Count, Acc) ->
	Num = list_to_binary(integer_to_list(Count)),
	Key = {<<"Attribute.", Num/binary, ".Name">>, K}, 
	Value =	{<<"Attribute.", Num/binary, ".Value">>, V},
	make_attribute_parameters(T, Count + 1, [Value, Key|Acc]);
%
make_attribute_parameters([], _, Acc) ->
	lists:reverse(Acc).

%% @public 
%% Put one or more attributes to one or more items
batch_put_attributes(Domain, ItemAttributeTree) ->
	ItemAttributeList = make_batch_attribute_parameters(ItemAttributeTree),
	request_action(#sdb_request{
		action = <<"BatchPutAttributes">>,
		params = [{<<"DomainName">>, Domain} | ItemAttributeList],
		meta = [request_id, box_usage]
	}).
%%
make_batch_attribute_parameters(Attributes) ->
	make_batch_attribute_parameters(Attributes, 1, []).
%
make_batch_attribute_parameters([{K, V}|T], Count, Acc) ->
	Num = list_to_binary(integer_to_list(Count)),
	Prefix = <<"Item.", Num/binary, ".">>,
	Key = {<<Prefix/binary, "ItemName">>, K},
	Values = make_batch_attribute_parameter_list(Prefix, V, 1, []),
	make_batch_attribute_parameters(T, Count + 1, lists:append([Acc, [Key | Values]]));
%
make_batch_attribute_parameters([], _, Acc) ->
	Acc.
%
make_batch_attribute_parameter_list(Prefix, [{K, V}| T], Count, Acc) ->
	Num = list_to_binary(integer_to_list(Count)),
	Key = {<<Prefix/binary, "Attribute.", Num/binary, ".Name">>, K}, 
	Value =	{<<Prefix/binary, "Attribute.", Num/binary, ".Value">>, V},
	make_batch_attribute_parameter_list(Prefix, T, Count + 1, [Value, Key|Acc]);
%
make_batch_attribute_parameter_list(_, [], _, Acc) ->
	lists:reverse(Acc).
	
%% @public
%% Delete all attributes associated with the item. 
delete_attributes(Domain, Item) ->
	delete_attributes(Domain, Item, []).
%% Delete one or more attributes associated with the item. 
delete_attributes(Domain, Item, Attributes) when is_binary(Domain), 
		is_binary(Item), is_list(Attributes) ->
	AttributeList = make_attribute_name_parameters(Attributes),
	request_action(#sdb_request{
		action = <<"DeleteAttributes">>, 
		params = [{<<"DomainName">>, Domain}, {<<"ItemName">>, Item} | AttributeList],
		meta = [request_id, box_usage]
	}).
%%
make_attribute_name_parameters(Attributes) ->
	make_attribute_name_parameters(Attributes, 1, []).
%
make_attribute_name_parameters([H|T], Count, Acc) ->
	Num = list_to_binary(integer_to_list(Count)),
	Value = {<<"Attribute.", Num/binary, ".Name">>, H}, 
	make_attribute_name_parameters(T, Count + 1, [Value|Acc]);
%
make_attribute_name_parameters([], _, Acc) ->
	lists:reverse(Acc).	

%% 
parse_option({max_items, X}) when is_integer(X) ->
	{<<"MaxNumberOfItems">>, list_to_binary(integer_to_list(X))};
parse_option({max_domains, X}) when is_integer(X) ->
	{<<"MaxNumberOfDomains">>, list_to_binary(integer_to_list(X))};
parse_option({next_token, X}) when is_binary(X), size(X) =:= ?KEY_SIZE ->
	{<<"NextToken">>, X}.

%%
metadata(ResponseType, MetadataType, Xml) when is_atom(MetadataType) ->
	metadata(ResponseType, [MetadataType], Xml);
metadata(ResponseType, MetadataTypes, Xml) ->
	metadata(ResponseType, MetadataTypes, Xml, []).
%
metadata(ResponseType, [H|T], Xml, Acc) ->
	Type = metadata_type(H),
	Path = <<"//", ResponseType/binary, "/ResponseMetadata/", Type/binary, "/text()">>,
	case ewok_xml:xpath(Path, Xml) of
	[] ->
		metadata(ResponseType, T, Xml, Acc);
	[Value] ->
		metadata(ResponseType, T, Xml, [{H, Value}|Acc]);
	Value ->
		metadata(ResponseType, T, Xml, [{H, Value}|Acc])
	end;
metadata(_, [], _, Acc) ->
	lists:reverse(Acc).
	
%%
metadata_type(request_id) -> <<"RequestId">>;
metadata_type(next_token) -> <<"NextToken">>;
metadata_type(box_usage)  -> <<"BoxUsage">>.

%%
parse_result(Path, Xml) ->
	[case P of [X] -> X; _ -> P end || P <- ewok_xml:xpath(Path, Xml)].

%%
request_action(#sdb_request{action = Action, result = Result, params = Params, options = Options, meta = Meta}) ->
	OptionParams = [parse_option(X) || X <- Options],
	RequestParams = lists:append(Params, OptionParams),
    case request(Action, RequestParams) of
	{ok, _Headers, Body} ->
		XmlDoc = ewok_xml:decode(Body),
%		?TTY(XmlDoc),
		Metadata = metadata(<<Action/binary, "Response">>, Meta, XmlDoc),
		Values = parse_result(<<"//", Action/binary, "Response/", 
			Action/binary, "Result/", Result/binary, "/values()">>, XmlDoc),
		{ok, Metadata, Values};
	{Status, _Headers, Body} ->
		parse_error(Status, Body);
	{error, Reason} -> 
		{error, Reason}
    end.

%%
request(Action, Params) ->
	{ok, AccessKey, SecretKey} = gen_server:call(?SERVER, credentials),
    Timestamp = ewok_util:timestamp(),
	AllParams = [
		{<<"AWSAccessKeyId">>, AccessKey},
		{<<"Action">>, Action}, 
		{<<"SignatureVersion">>, <<"2">>},
		{<<"SignatureMethod">>, <<"HmacSHA256">>},
		{<<"Timestamp">>, Timestamp},
		{<<"Version">>, ?AWS_SDB_VERSION}
		| Params
	],
	%% TODO: This is untidy.
	{Protocol, Host} = 
		case ?SECURE of 
		true -> 
			{<<"https://">>, <<?AWS_SDB_HOST/binary, ":443">>};
		false -> 
			{<<"http://">>, <<?AWS_SDB_HOST/binary, ":80">>}
		end,
	StringToSign = 
		list_to_binary([<<"GET\n">>, Host, <<"\n/\n">>, make_querystring(lists:sort(AllParams), [])]),
	Signature = ewok_crypto:sign(hmac256, SecretKey, StringToSign),
	FinalParams = AllParams ++ [{<<"Signature">>, base64:encode(Signature)}],
    Url = list_to_binary([Protocol, Host, <<"/?">>, make_querystring(FinalParams, [])]),
	ewok_http_client:get(Url).
	
%%
make_querystring([{K, V}], Acc) -> 
	Param = list_to_binary([ewok_http:url_encode(K), <<"=">>, ewok_http:url_encode(V)]),
	make_querystring([], [Param | Acc]);
make_querystring([{K, V}|T], Acc) -> 
	Param = list_to_binary([ewok_http:url_encode(K), <<"=">>, ewok_http:url_encode(V), <<"&">>]),
	make_querystring(T, [Param | Acc]);
make_querystring([], Acc) ->
	list_to_binary(lists:reverse(Acc)).

%%
parse_error(Status, Body) ->
    XmlDoc = ewok_xml:decode(Body),
%	?TTY(XmlDoc),
    [ErrorCode|_] = ewok_xml:xpath("//Response/Errors/Error/Code/text()", XmlDoc),
    [ErrorMessage|_] = ewok_xml:xpath("//Response/Errors/Error/Message/text()", XmlDoc),
    [RequestId|_] = ewok_xml:xpath("//Response/RequestID/text()", XmlDoc),
    {Status, [{request_id, RequestId}], [{error_code(ErrorCode), ErrorMessage}]}.

error_code(<<"InvalidAction">>) -> invalid_action;
error_code(Code) -> Code.

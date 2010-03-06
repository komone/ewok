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

-module(ewok_sdb_v1).
-include("ewok.hrl").

-define(AWS_SDB_ACCESS_KEY, <<"">>).
-define(AWS_SDB_SECRET_KEY, <<"">>).

-export([list_domains/0, list_domains/1, create_domain/1, delete_domain/1,
	get_attributes/2, put_attributes/3, delete_attributes/3,
	list_items/1, list_items/2, delete_item/2,
	query_items/2, query_items/3]).
%% ??
-export([parse_error/1]).

-define(AWS_SDB_HOST, <<"sdb.amazonaws.com">>).
-define(AWS_SDB_VERSION, <<"2007-11-07">>).

-define(KEY_SIZE, 36).
-define(SECURE, false).

%-behaviour(ewok_datasource).

-record(sdb_request, {action, result = <<>>, params = [], options = [], meta = []}).

%%
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

%% This function deletes a SimpleDB domain. Any items (and their attributes) in the domain 
%% are deleted as well. This function might take 10 or more seconds to complete.
%%  -> "MissingParameter"
delete_domain(Domain) when is_binary(Domain) ->
	request_action(#sdb_request{
		action = <<"DeleteDomain">>, 
		result = <<"DomainName">>,
		params = [{<<"DomainName">>, Domain}],
		meta = [request_id, box_usage]
	}).

%% Returns a list of all items of a domain - 100 at a time. If your
%% domains contains more then 100 item you must use list_items/2 to
%% retrieve all items.
list_items(Domain) ->
    list_items(Domain, []).
%% Returns up to max_items -> integer() <= 250 items of a domain. If
%% the total item count exceeds max_items you must call this function
%% again with the NextToken value provided in the return value.
list_items(Domain, Options) when is_binary(Domain), is_list(Options) ->
	request_action(#sdb_request{
		action = <<"Query">>, 
		result = <<"ItemName">>,
		params = [{<<"DomainName">>, Domain}],
		options = Options,
		meta = [request_id, next_token, box_usage]
	}).

%% Deletes the specified item. 
%%  F -> "InvalidParameterValue" | "MissingParameter" | "NoSuchDomain"
delete_item(Domain, Item) ->
    delete_attributes(Domain, Item, []).

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
	AttributeList = make_attribute_parameters(Attributes, 0, []),
	request_action(#sdb_request{
		action = <<"PutAttributes">>, 
		params = [{<<"DomainName">>, Domain}, {<<"ItemName">>, Item} | AttributeList],
		meta = [request_id, box_usage]
	}).

%% Deletes one or more attributes associated with the item. 
delete_attributes(Domain, Item, Attributes) when is_binary(Domain), 
		is_binary(Item), is_list(Attributes) ->
	AttributeList = make_attribute_parameters(Attributes, 0, []),
	request_action(#sdb_request{
		action = <<"DeleteAttributes">>, 
		params = [{<<"DomainName">>, Domain}, {<<"ItemName">>, Item} | AttributeList],
		meta = [request_id, box_usage]
	}).

%%
make_attribute_parameters([{K, V}|T], Count, Acc) ->
	Num = list_to_binary(integer_to_list(Count)),
	Key = {<<"Attribute.", Num/binary, ".Name">>, K}, 
	Value =	{<<"Attribute.", Num/binary, ".Value">>, V},
	make_attribute_parameters(T, Count + 1, [Value, Key|Acc]);
make_attribute_parameters([], _, Acc) ->
	lists:reverse(Acc).

%% Executes the given query expression against a domain. The syntax for
%% such a query spec is documented here:
%% http://docs.amazonwebservices.com/AmazonSimpleDB/2007-11-07/DeveloperGuide/SDB_API_Query.html
%%
%% Spec: query_items(Domain::string(), QueryExp::string()]) ->
%%       {ok, Items::[string()], []} |
%%       {ok, Items::[string()], NextToken::string()} |
%%       {error, {Code::string(), Msg::string(), ReqId::string()}}
%%
%%       Code::string() -> "InvalidParameterValue" | "InvalidNextToken" | 
%%                         "MissingParameter" | "NoSuchDomain"
%%  
query_items(Domain, QueryExp) ->
    query_items(Domain, QueryExp, []).

%% Executes the given query expression against a domain. The syntax for
%% such a query spec is documented here:
%% http://docs.amazonwebservices.com/AmazonSimpleDB/2007-11-07/DeveloperGuide/SDB_API_Query.html
query_items(Domain, QueryExp, Options) when is_list(Options) ->     
	Params = [{<<"QueryExpression">>, QueryExp} | [parse_option(X) || X <- Options]],
	case request(<<"Query">>, Domain, "", [], Params) of
    {ok, Body} -> 
		XmlDoc = ewok_xml:decode(Body),
		ItemNodes = ewok_xml:xpath("//QueryResponse/QueryResult/ItemName/values()", XmlDoc),
		Metadata = metadata(<<"QueryResponse">>, [request_id, next_token, box_usage], XmlDoc),
		{ok, Metadata, ItemNodes};
	{error, Reason} -> 
		{error, Reason}
    end.

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
	{error, Reason} -> 
		{error, Reason}
    end.

%% For removal
request(Action, _, _, _, Options) ->
	request(Action, Options).
%%
request(Action, Params) ->
    Timestamp = ewok_util:timestamp(),
	AllParams = [
		{<<"AWSAccessKeyId">>, ?AWS_SDB_ACCESS_KEY},
		{<<"Action">>, Action}, 
		{<<"Version">>, ?AWS_SDB_VERSION},
		{<<"SignatureVersion">>, <<"1">>},
		{<<"Timestamp">>, Timestamp}
		| Params
	],
	StringToSign = 
		list_to_binary([<<Param/binary, Value/binary>> || {Param, Value} <- lists:sort(fun comparator/2, AllParams)]),
	?TTY({string_to_sign, StringToSign}),
    Signature = 
		base64:encode(crypto:sha_mac(?AWS_SDB_SECRET_KEY, StringToSign)),
    FinalParams = AllParams ++ [{<<"Signature">>, Signature}],
	Protocol = 
		case ?SECURE of 
		true -> <<"https://">>;
		false -> <<"http://">> 
		end,
    Url = list_to_binary([Protocol, ?AWS_SDB_HOST, make_querystring(FinalParams, [])]),
%	?TTY(Url),
	ewok_http_client:get(Url).
	
%%
comparator({X, _}, {Y, _}) ->
	ewok_text:to_lower(X) =< ewok_text:to_lower(Y).

%%
make_querystring([{K, V}], Acc) -> 
	Param = list_to_binary([ewok_http:url_encode(K), <<"=">>, ewok_http:url_encode(V)]),
	make_querystring([], [Param | Acc]);
make_querystring([{K, V}|T], Acc) -> 
	Param = list_to_binary([ewok_http:url_encode(K), <<"=">>, ewok_http:url_encode(V), <<"&">>]),
	make_querystring(T, [Param | Acc]);
make_querystring([], Acc) ->
	list_to_binary([<<"/?">>, lists:reverse(Acc)]).

%%
parse_error(Xml) ->
    XmlDoc = ewok_xml:decode(Xml),
    [ErrorCode|_] = ewok_xml:xpath("//Error/Code/text()", XmlDoc),
    [ErrorMessage|_] = ewok_xml:xpath("//Error/Message/text()", XmlDoc),
    [RequestId|_] = ewok_xml:xpath("//RequestID/text()", XmlDoc),
    {ErrorCode, ErrorMessage, RequestId}.

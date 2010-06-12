%%
-include("amqp.hrl").
-include("ewok_rabbit.hrl").

-define(PROTOCOL_HEADER, <<"AMQP", 1, 1, ?PROTOCOL_VERSION_MAJOR, ?PROTOCOL_VERSION_MINOR>>).

-record(amqp_msg, {props = #'P_basic'{}, payload = <<>>}).

-record(amqp_params, {username = <<"guest">>, password = <<"guest">>,
	virtual_host = <<"/">>, host = "localhost", port = ?PROTOCOL_PORT, ssl_options  = none}).

-record(rpc_client_state, {channel, reply_queue, exchange, routing_key,
	continuations = dict:new(), correlation_id = 0}).

-record(rpc_server_state, {channel, handler}).

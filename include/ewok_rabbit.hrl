%% ewok_rabbit.hrl

-define(COPYRIGHT_MESSAGE, "Copyright (C) 2007-2009 LShift Ltd., Cohesive Financial Technologies LLC., and Rabbit Technologies Ltd.").
-define(INFORMATION_MESSAGE, "Licensed under the MPL.  See http://www.rabbitmq.com/").

-record(user, {username, password}).
-record(permission, {configure, write, read}).
-record(user_vhost, {username, virtual_host}).
-record(user_permission, {user_vhost, permission}).

-record(vhost, {virtual_host, dummy}).

-record(connection, {user, timeout_sec, frame_max, vhost}).

-record(content,
        {class_id,
         properties, %% either 'none', or a decoded record/tuple
         properties_bin, %% either 'none', or an encoded properties binary
         %% Note: at most one of properties and properties_bin can be
         %% 'none' at once.
         payload_fragments_rev %% list of binaries, in reverse order (!)
         }).

-record(resource, {virtual_host, kind, name}).

-record(exchange, {name, type, durable, auto_delete, arguments}).

-record(amqqueue, {name, durable, auto_delete, arguments, pid}).

%% mnesia doesn't like unary records, so we add a dummy 'value' field
-record(route, {binding, value = const}).
-record(reverse_route, {reverse_binding, value = const}).

-record(binding, {exchange_name, key, queue_name, args = []}).
-record(reverse_binding, {queue_name, key, exchange_name, args = []}).

-record(listener, {node, protocol, host, port}).

-record(basic_message, {exchange_name, routing_key, content, persistent_key}).

-record(ssl_socket, {tcp, ssl}).
-record(delivery, {mandatory, immediate, txn, sender, message}).

-record(amqp_error, {name, explanation, method = none}).




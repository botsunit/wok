-define(DEFAULT_MESSAGE_MAX_BYTES, 10485760).
-define(DEFAULT_FETCH_FREQUENCY, 5000).
-define(DEFAULT_MAX_MESSAGES, 1000).
-define(DEFAULT_MAX_SERVICES_FORK, 10).
-define(DEFAULT_REST_PORT, 8080).
-define(DEFAULT_REST_IP, "0.0.0.0").
-define(DEFAULT_REST_MAX_CONN, 100).
-define(DEFAULT_MESSAGE_HANDLER, wok_message).
-define(DEFAULT_LOCAL_QUEUE, <<"local_queue">>).

-record(wok_msg_resp, {
          reply = false,
          from = undefined,
          to = undefined,
          topic = undefined,
          body = <<>>
         }).

-record(wok_msg, {
          message = undefined,
          response = #wok_msg_resp{},
          global_state = undefined,
          local_state = undefined,
          custom_data = undefined
         }).

-record(message_transfert, {
          key
          , message
          , partition
          , topic
          , service
          , action
          , local_queue
          , service_name
          , consume_method
         }).


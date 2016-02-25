-define(DEFAULT_MESSAGE_MAX_BYTES, 10485760).
-define(DEFAULT_FETCH_FREQUENCY, 5000).
-define(DEFAULT_MAX_MESSAGES, 1000).
-define(DEFAULT_MAX_SERVICES_FORK, 10).
-define(DEFAULT_REST_PORT, 8080).
-define(DEFAULT_REST_IP, "0.0.0.0").
-define(DEFAULT_REST_MAX_CONN, 100).
-define(DEFAULT_MESSAGE_HANDLER, wok_message).
-define(DEFAULT_LOCAL_QUEUE, <<"local_queue">>).

-record(message_transfert, {
          key,
          message,
          result,
          partition,
          topic,
          service,
          action,
          local_queue,
          service_name,
          consume_method
         }).

-record(wok_resp, {
    code = 200
    , headers = []
    , body = <<>>
  }).

-record(wok_req, {
    req,
    ,custom_data = undefined
    ,global_state = undefined
    ,local_state = undefined
    ,response = #wok_resp{}
  }).


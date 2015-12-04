% @hidden
-module(wok_app).
-compile([{parse_transform, lager_transform}]).
-behaviour(application).
-include("../include/wok.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  _ = check_message_handler(),
  _ = start_rest(),
  _ = start_messages(),
  wok_sup:start_link().

stop(_State) ->
  ok.

start_rest() ->
  case doteki:get_env([wok, rest]) of
    undefined ->
      lager:debug("No REST configuration.");
    _ ->
      _ = application:ensure_all_started(cowboy),
      Port = doteki:get_env([wok, rest, port], ?DEFAULT_REST_PORT),
      IP = bucinet:to_ip(doteki:get_env([wok, rest, ip], ?DEFAULT_REST_IP)),
      TransOpts = [{port, Port}, {ip, IP}],
      MaxConn = doteki:get_env([wok, rest, max_conn], ?DEFAULT_REST_MAX_CONN),
      Dispatch = wok_rest_handler:routes(
                   doteki:get_env([wok, rest, routes], []) ++
                   wok_middlewares:routes()
                  ),
      ProtoOpts   = [{env, [{dispatch, Dispatch}]}],
      case cowboy:start_http(http, MaxConn, TransOpts, ProtoOpts) of
        {ok, _} ->
          lager:info("Start HTTP on port ~p", [Port]);
        _ ->
          lager:error("Faild to start HTTP server"),
          exit(http_error)
      end
  end.

start_messages() ->
  case doteki:get_env([wok, messages]) of
    undefined ->
      ok;
    _ ->
      _ = start_local_queue(),
      _ = application:ensure_all_started(kafe),
      ok
  end.

check_message_handler() ->
  Handler = doteki:get_env([wok, messages, handler], ?DEFAULT_MESSAGE_HANDLER),
  case code:ensure_loaded(Handler) of
    {module, Handler} ->
      lager:info("Message handler ~p loaded", [Handler]);
    {error, What} ->
      lager:error("Can't load handler ~p", [Handler]),
      exit(What)
  end.

start_local_queue() ->
  LocalQueue = bucs:to_atom(doteki:get_env([wok, messages, local_queue_name], ?DEFAULT_LOCAL_QUEUE)),
  case pipette:queue(LocalQueue) of
    missing_queue ->
      case pipette:new_queue(LocalQueue) of
        {ok, _} ->
          lager:info("Local queue ~p created", [LocalQueue]);
        {error, Reason} ->
          lager:error("Faild to create local queue ~p: ~p", [LocalQueue, Reason]),
          exit(Reason)
      end;
    _ ->
      lager:info("Local queue ~p started", [LocalQueue])
  end.


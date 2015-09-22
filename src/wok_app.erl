% @hidden
-module(wok_app).
-compile([{parse_transform, lager_transform}]).
-behaviour(application).
-include("../include/wok.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  _ = check_message_handler(),
  _ = start_local_queue(),
  _ = start_rest(),
  _ = start_messages(),
  wok_sup:start_link().

stop(_State) ->
  ok.

start_rest() ->
  case wok_config:conf([wok, rest]) of
    undefined ->
      ok;
    _ ->
      _ = application:ensure_all_started(cowboy),
      Port = wok_config:conf([wok, rest, port], ?DEFAULT_REST_PORT),
      IP = enet:str_to_ip(wok_config:conf([wok, rest, ip], ?DEFAULT_REST_IP)),
      TransOpts = [{port, Port}, {ip, IP}],
      MaxConn = wok_config:conf([wok, rest, max_conn], ?DEFAULT_REST_MAX_CONN),
      Dispatch = wok_rest_handler:routes(
                   wok_config:conf([wok, rest, routes], []) ++
                   wok_middlewares:routes()
                  ),
      ProtoOpts   = [{env, [{dispatch, Dispatch}]}],
      {ok, _} = cowboy:start_http(http, MaxConn, TransOpts, ProtoOpts)
  end.

start_messages() ->
  case wok_config:conf([wok, messages]) of
    undefined ->
      ok;
    _ ->
      _ = application:ensure_all_started(kafe),
      ok
  end.

check_message_handler() ->
  Handler = wok_config:conf([wok, messages, handler], ?DEFAULT_MESSAGE_HANDLER),
  case code:ensure_loaded(Handler) of
    {module, Handler} ->
      ok;
    {error, What} ->
      lager:info("Can't load handler ~p", [Handler]),
      exit(What)
  end.

start_local_queue() ->
  LocalQueue = eutils:to_atom(wok_config:conf([wok, messages, local_queue_name], ?DEFAULT_LOCAL_QUEUE)),
  case pipette:queue(LocalQueue) of
    missing_queue ->
      case pipette:new_queue(LocalQueue) of
        {ok, _} ->
          lager:info("Local queue ~p created", [LocalQueue]);
        {error, Reason} ->
          lager:info("Faild to create local queue ~p: ~p", [LocalQueue, Reason])
      end;
    _ ->
      ok
  end.


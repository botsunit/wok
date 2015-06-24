% @hidden
-module(wok_app).

-behaviour(application).

-include("../include/wok.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
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
      Dispatch = wok_rest_handler:routes(wok_config:conf([wok, rest, routes], [])),
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

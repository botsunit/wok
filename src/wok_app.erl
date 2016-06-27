% @hidden
-module(wok_app).
-compile([{parse_transform, lager_transform}]).
-behaviour(application).
-include("../include/wok.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  Static = start_rest(),
  _ = start_messages(),
  wok_sup:start_link(Static).

stop(_State) ->
  ok.

start_rest() ->
  case doteki:get_env([wok, rest]) of
    undefined ->
      lager:debug("No REST configuration."),
      #{};
    _ ->
      _ = application:ensure_all_started(cowboy),
      Port = doteki:get_env([wok, rest, port], ?DEFAULT_REST_PORT),
      IP = bucinet:to_ip(doteki:get_env([wok, rest, ip], ?DEFAULT_REST_IP)),
      TransOpts = [{port, Port}, {ip, IP}],
      MaxConn = doteki:get_env([wok, rest, max_conn], ?DEFAULT_REST_MAX_CONN),
      {Dispatch, Static} = wok_cowboy_handler:routes(),
      ProtoOpts   = [{env, [{dispatch, Dispatch}]},
                     {middlewares, [cowboy_router, cowboy_default_static_file, cowboy_handler]}],
      case cowboy:start_http(http, MaxConn, TransOpts, ProtoOpts) of
        {ok, _} ->
          lager:info("Start HTTP on port ~p", [Port]),
          Static;
        _ ->
          lager:error("Faild to start HTTP server"),
          exit(http_error)
      end
  end.

start_messages() ->
  case doteki:get_env([wok, messages]) of
    undefined ->
      lager:info("No message configuration");
    _ ->
      Handler = case doteki:get_env([wok, messages, handler], ?DEFAULT_MESSAGE_HANDLER) of
                  {Module, _} -> Module;
                  Module -> Module
                end,
      case code:ensure_loaded(Handler) of
        {module, Handler} ->
          lager:info("Message handler ~p loaded", [Handler]);
        {error, Reason} ->
          lager:error("Can't load handler ~p: ~p", [Handler, Reason]),
          init:stop()
      end,
      _ = pipette:clean_all(),
      _ = application:ensure_all_started(kafe),
      ok
  end.


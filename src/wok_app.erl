% @hidden
-module(wok_app).
-compile([{parse_transform, lager_transform}]).
-behaviour(application).
-include("../include/wok.hrl").

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  start_apps(),
  Static = start_rest(),
  _ = start_messages(),
  wok_sup:start_link(Static).

stop(_State) ->
  ok.

start_apps() ->
  start_apps(
    doteki:get_env([wok, start], [])).

start_apps([]) ->
  ok;
start_apps([{Application, Type, application}|Rest]) when Type == permanent;
                                                         Type == transient;
                                                         Type == temporary ->
  application:ensure_all_started(Application, Type),
  start_apps(Rest);
start_apps([{Application, application}|Rest]) ->
  application:ensure_all_started(Application),
  start_apps(Rest);
start_apps([_|Rest]) ->
  start_apps(Rest).

start_rest() ->
  case bucs:function_exists(wok_rest_initializer, start, 0) of
    true ->
      wok_rest_initializer:start();
    false ->
      #{}
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
      _ = application:ensure_all_started(kafe),
      ok
  end.


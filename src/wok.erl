% @hidden
-module(wok).
-include("../include/wok.hrl").

-export([start/0]).
-export([provide/5, provide/4]).

start() ->
  application:ensure_all_started(?MODULE).

provide(Topic, From, To, Body) ->
  provide(Topic, From, To, Body, []).

provide(Topic, From, To, Body, Options) ->
  Message = erlang:apply(wok_config:conf([wok, messages, handler], 
                                         ?DEFAULT_MESSAGE_HANDLER), 
                         create, [From, To, Body, Options]),
  kafe:produce(Topic, Message).

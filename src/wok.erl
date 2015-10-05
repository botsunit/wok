% @hidden
-module(wok).
-include("../include/wok.hrl").

-export([start/0]).
-export([provide/5, provide/4, provide/2]).

start() ->
  application:ensure_all_started(?MODULE).

provide(Topic, From, To, Body) ->
  wok_producer:provide(Topic, From, To, Body).

provide(Topic, From, To, Body, Options) ->
  wok_producer:provide(Topic,  From, To, Body, Options).

provide(Topic, Message) ->
  wok_producer:provide(Topic, Message).


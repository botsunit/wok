% @hidden
-module(dummy_initializer).
-behaviour(wok_initializer).

-export([init/1]).

init(Args) ->
  {ok, Args}.

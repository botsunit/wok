% @hidden
-module(dummy_initializer).
-behaviour(wok_initializer).

-export([init/1, terminate/2]).

init(Args) ->
  {ok, Args}.

terminate(_Reason, _State) ->
  ok.


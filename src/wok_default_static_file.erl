-module(wok_default_static_file).
-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
  {ok, Req, Env}.

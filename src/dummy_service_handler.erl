-module(dummy_service_handler).

-export([my_service/1, my_service_post/1]).

my_service(_Req) ->
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello GET">>}.

my_service_post(_Req) ->
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello POST">>}.

-module(dummy_service_handler).

-export([my_service/1]).
-export([my_service_get/1, my_service_post/1]).

my_service(Message) ->
  noreply.

my_service_get(_Req) ->
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello GET">>}.

my_service_post(_Req) ->
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello POST">>}.

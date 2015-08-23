% @hidden
-module(dummy_service_handler).
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-export([my_service/1]).
-export([my_service_get/1, my_service_post/1, my_service_get2/1]).

my_service(Message) ->
  lager:info("BEGIN dummy_service_handler =>>>>>>>>>< ~p", [Message]),
  timer:sleep(2000 + random:uniform(4000)),
  lager:info("END dummy_service_handler =>>>>>>>>>< ~p", [Message]),
  noreply.

my_service_get(_Req) ->
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello GET">>}.

my_service_post(_Req) ->
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello POST">>}.

my_service_get2(_Req) ->
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello GET2">>}.


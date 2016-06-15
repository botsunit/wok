% @hidden
-module(dummy_producer_handler).
-behaviour(wok_producer).

-export([messages/3, response/2]).

messages(_Topic, _Partition, _NumMessage) ->
  [].

response(_MessageID, _Response) ->
  next.

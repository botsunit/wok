% @hidden
-module(dummy_service_handler).
-compile([{parse_transform, lager_transform}]).
% -include_lib("wok_message_handler/include/wok_message_handler.hrl").

-export([my_action/1, my_async_action/1, my_answer/1, parameterized_action/1]).
-export([my_service_get/1, my_service_post/1, my_service_get2/1]).
-export([ws_init/1, ws_handle/2, ws_info/2]).

-define(RAND, begin
                case code:ensure_loaded(rand) of
                  {module, rand} -> rand;
                  _ -> random
                end
              end).

% controler

my_action(Message) ->
  lager:info("BEGIN dummy_service_handler:my_action =>>>>>>>>>< ~p :: ~p",
             [wok_message:content(Message), wok_message:global_state(Message)]),
  timer:sleep(2000 + erlang:apply(?RAND, uniform, [4000])),
  lager:info("END dummy_service_handler:my_action =>>>>>>>>>< ~p",
             [wok_message:content(Message)]),
  wok_message:reply(Message,
                    {<<"test">>, 1},
                    <<"my_service/my_controler/my_answer">>,
                    <<"Message response from my_action">>).

my_async_action(Message) ->
  lager:info("BEGIN dummy_service_handler:my_action =>>>>>>>>>< ~p :: ~p",
             [wok_message:content(Message), wok_message:global_state(Message)]),
  timer:sleep(2000 + erlang:apply(?RAND, uniform, [4000])),
  case wok_message:encode_reply(Message,
                                {<<"test">>, <<"mYk3Y">>},
                                <<"my_service/my_controler/my_answer">>,
                                <<"Message response from my_async_action">>) of
    {ok, Topic, Partition, MessageTransfert} ->
      lager:info("STORE ~p for ~p#~p", [MessageTransfert, Topic, Partition]),
      dummy_producer_handler:store(Topic, Partition, MessageTransfert);
    {error, Reason} ->
      lager:info("ERROR !!!! Encode response faild: ~p", [Reason])
  end,

  lager:info("END dummy_service_handler:my_action =>>>>>>>>>< ~p",
             [wok_message:content(Message)]),
  wok_message:async_reply(Message).

my_answer(Message) ->
  lager:info("BEGIN dummy_service_handler:my_answer =>>>>>>>>>< ~p :: ~p",
             [wok_message:content(Message), wok_message:global_state(Message)]),
  timer:sleep(2000 + erlang:apply(?RAND, uniform, [4000])),
  lager:info("END dummy_service_handler:my_answer =>>>>>>>>>< ~p",
             [wok_message:content(Message)]),
  wok_message:noreply(Message).

parameterized_action(Message) ->
  lager:info("====> ~p", [wok_message:params(Message)]),
  wok_message:noreply(Message).

% rest

my_service_get(WokReq) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  State = wok_request:local_state(WokReq),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  wok_response:set_response(WokReq, {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello GET">>}).

my_service_post(WokReq) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  State = wok_request:local_state(WokReq),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  wok_response:set_response(WokReq, {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello POST">>}).

my_service_get2(WokReq) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  State = wok_request:local_state(WokReq),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  wok_response:set_response(WokReq, {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello GET2">>}).

% websocket

ws_init(Req) ->
  erlang:start_timer(1000, wok_request:handler(Req), <<"Hello!">>),
  {ok, Req}.

ws_handle({text, Msg}, Req) ->
  {reply, {text, << "That's what she said! ", Msg/binary >>}, Req};
ws_handle(_Data, Req) ->
  {ok, Req}.

ws_info({timeout, _Ref, Msg}, Req) ->
  erlang:start_timer(1000, wok_request:handler(Req), <<"How' you doin'?">>),
  {reply, {text, Msg}, Req};
ws_info(_Info, Req) ->
  {ok, Req}.


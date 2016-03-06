% @hidden
-module(dummy_service_handler).
-compile([{parse_transform, lager_transform}]).
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-export([my_action/2, my_answer/2]).
-export([my_service_get/1, my_service_post/1, my_service_get2/1]).
-export([ws_init/1, ws_handle/2, ws_info/2]).

% controler

my_action(Message, State) ->
  lager:info("BEGIN dummy_service_handler:my_action =>>>>>>>>>< ~p :: ~p", [Message, State]),
  timer:sleep(2000 + random:uniform(4000)),
  lager:info("END dummy_service_handler:my_action =>>>>>>>>>< ~p", [Message]),
  {reply, {<<"test">>, 1}, {<<"my_service/my_controler/my_answer">>, <<"Message response from my_action">>}, State}.

my_answer(Message, State) ->
  lager:info("BEGIN dummy_service_handler:my_answer =>>>>>>>>>< ~p :: ~p", [Message, State]),
  timer:sleep(2000 + random:uniform(4000)),
  lager:info("END dummy_service_handler:my_answer =>>>>>>>>>< ~p", [Message]),
  {noreply, State}.

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


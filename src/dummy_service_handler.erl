% @hidden
-module(dummy_service_handler).
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-export([my_action/2]).
-export([my_answer/2]).
-export([my_service_get/2, my_service_post/2, my_service_get2/2]).

my_action(Message, State) ->
  lager:info("BEGIN dummy_service_handler:my_action =>>>>>>>>>< ~p :: ~p", [Message, State]),
  timer:sleep(2000 + random:uniform(4000)),
  lager:info("END dummy_service_handler:my_action =>>>>>>>>>< ~p", [Message]),
  {reply, <<"test">>, {<<"my_service/my_controler/my_answer">>, <<"Message response from my_action">>}, State}.

my_answer(Message, State) ->
  lager:info("BEGIN dummy_service_handler:my_answer =>>>>>>>>>< ~p :: ~p", [Message, State]),
  timer:sleep(2000 + random:uniform(4000)),
  lager:info("END dummy_service_handler:my_answer =>>>>>>>>>< ~p", [Message]),
  {noreply, State}.

my_service_get(_Req, State) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello GET">>, State}.

my_service_post(_Req, State) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello POST">>, State}.

my_service_get2(_Req, State) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Hello GET2">>, State}.


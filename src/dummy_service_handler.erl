% @hidden
-module(dummy_service_handler).
-compile([{parse_transform, lager_transform}]).
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-export([my_action/2, my_answer/2]).
-export([my_service_get/2, my_service_post/2, my_service_get2/2]).
-export([ws_init/3, ws_handle/4, ws_info/4]).

% controler

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

% rest

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

% websocket

ws_init(Req, Handler, State) ->
  erlang:start_timer(1000, Handler, <<"Hello!">>),
  {ok, Req, State}.

ws_handle({text, Msg}, Req, _Handler, State) ->
  {reply, {text, << "That's what she said! ", Msg/binary >>}, Req, State};
ws_handle(_Data, Req, _Handler, State) ->
  {ok, Req, State}.

ws_info({timeout, _Ref, Msg}, Req, Handler, State) ->
  erlang:start_timer(1000, Handler, <<"How' you doin'?">>),
  {reply, {text, Msg}, Req, State};
ws_info(_Info, Req, _Handler, State) ->
  {ok, Req, State}.


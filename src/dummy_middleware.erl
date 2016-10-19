% @hidden
-module(dummy_middleware).
-compile([{parse_transform, lager_transform}]).

-export([init/1, routes/0]).
-export([incoming_message/1, outgoing_message/1]).
-export([incoming_http/1, outgoing_http/1]).
-export([my_dummy_get/1, my_dummy_post/1]).

init(Args) ->
  lager:info("Initialize middleware ~p", [?MODULE]),
  {ok, Args}. % {stop, Reason}

routes() ->
  [
   {'GET', "/dummy_get", {?MODULE, my_dummy_get}},
   {'POST', "/dummy_post", {?MODULE, my_dummy_post}}
  ].

incoming_message(Message) ->
  lager:info("Middleware incoming ~p was called - message: ~p - state: ~p", [?MODULE, wok_message:content(Message), wok_message:local_state(Message)]),
  {ok, Message}. % {stop, Reason, Message}.

outgoing_message(Message) ->
  lager:info("Middleware outgoing ~p was called - message: ~p - state: ~p", [?MODULE, wok_message:response(Message), wok_message:local_state(Message)]),
  {ok, Message}. % {stop, Reason, Message}.

incoming_http(WokReq) ->
  {continue, WokReq}. % WokReq

outgoing_http(WokReq) ->
  WokReq.

my_dummy_get(WokReq) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  State = wok_request:local_state(WokReq),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  wok_response:set_response(WokReq, {200, [{<<"content-type">>, <<"text/plain">>}], <<"Dummy GET">>}).

my_dummy_post(WokReq) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  State = wok_request:local_state(WokReq),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  wok_response:set_response(WokReq, {200, [{<<"content-type">>, <<"text/plain">>}], <<"Dummy POST">>}).

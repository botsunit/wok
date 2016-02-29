% @hidden
-module(dummy_middleware).
-compile([{parse_transform, lager_transform}]).

-export([init/1, routes/0]).
-export([incoming_message/2, outgoing_message/2]).
-export([incoming_http/2, outgoing_http/2]).
-export([my_dummy_get/2, my_dummy_post/2]).

init(Args) ->
  lager:info("Initialize middleware ~p", [?MODULE]),
  {ok, Args}. % {stop, Reason}

routes() ->
  [
   {'GET', "/dummy_get", {?MODULE, my_dummy_get}},
   {'POST', "/dummy_post", {?MODULE, my_dummy_post}}
  ].

incoming_message(Message, State) ->
  lager:info("Middleware ingoing ~p was called - message: ~p - state: ~p", [?MODULE, Message, State]),
  {ok, Message, State}. % {stop, Reason, State}.

outgoing_message(Message, State) ->
  lager:info("Middleware outgoing ~p was called - message: ~p - state: ~p", [?MODULE, Message, State]),
  {ok, Message, State}. % {stop, Reason, State}.

incoming_http(WokReq) ->
  {continue, WokReq}. % {Code, Headers, Body, State}

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

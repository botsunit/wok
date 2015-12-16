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

incoming_http(Req, State) ->
  {continue, Req, State}. % {Code, Headers, Body, State}

outgoing_http({C, H, B}, State) ->
  {C, H, B, State}.

my_dummy_get(_Req, State) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Dummy GET">>, State}.

my_dummy_post(_Req, State) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Dummy POST">>, State}.

% @hidden
-module(dummy_middleware).
-compile([{parse_transform, lager_transform}]).

-export([init/1, routes/0]).
-export([ingoing/2, outgoing/2]).
-export([my_dummy_get/2, my_dummy_post/2]).

init(Args) ->
  lager:info("Initialize middleware ~p", [?MODULE]),
  {ok, Args}. % {stop, Reason}

routes() ->
  [
   {'GET', "/dummy_get", {?MODULE, my_dummy_get}},
   {'POST', "/dummy_post", {?MODULE, my_dummy_post}}
  ].

ingoing(Message, State) ->
  lager:info("Middleware ingoing ~p was called - message: ~p - state: ~p", [?MODULE, Message, State]),
  {ok, Message, State}. % {stop, Reason, State}.

outgoing(Message, State) ->
  lager:info("Middleware outgoing ~p was called - message: ~p - state: ~p", [?MODULE, Message, State]),
  {ok, Message, State}. % {stop, Reason, State}.

my_dummy_get(_Req, State) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Dummy GET">>, State}.

my_dummy_post(_Req, State) ->
  {current_function, {M, F, A}} = process_info(self(), current_function),
  lager:info("~p:~p/~p call with state = ~p", [M, F, A, State]),
  {200, [{<<"content-type">>, <<"text/plain">>}], <<"Dummy POST">>, State}.

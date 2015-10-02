% @hidden
-module(dummy_plugin).

-export([foo/1, bar/1, baz/1]).

foo(State) ->
  {ok, State}.

bar(State) ->
  {send, <<"topic">>, {<<"service/controler/action">>, <<"Message body">>}, State}.

baz(State) ->
  {stop, State}.

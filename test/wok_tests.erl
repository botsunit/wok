-module(wok_tests).

-include_lib("eunit/include/eunit.hrl").

wok_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
      ?_test(t_t())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_t() ->
  ?assert(true).


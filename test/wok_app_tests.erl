-module(wok_app_tests).
-include_lib("eunit/include/eunit.hrl").

wok_app_run_with_rest_test_() ->
  {setup,
   fun() ->
       meck:new(bucs),
       meck:expect(bucs, function_exists, 3, true),
       meck:new(wok_rest_initializer, [non_strict]),
       meck:expect(wok_rest_initializer, start, 0, static_for_tests),
       meck:new(doteki),
       meck:expect(doteki, get_env, 1, undefined),
       meck:expect(doteki, get_env, 2, []),
       meck:new(wok_sup),
       meck:expect(wok_sup, start_link, fun(X) -> X end)
   end,
   fun(_) ->
       meck:unload(wok_sup),
       meck:unload(doteki),
       meck:unload(wok_rest_initializer),
       meck:unload(bucs)
   end,
   [
    fun() ->
        ?assertEqual(static_for_tests, wok_app:start(x, y))
    end
   ]}.

wok_app_run_without_rest_test_() ->
  {setup,
   fun() ->
       meck:new(bucs),
       meck:expect(bucs, function_exists, 3, false),
       meck:new(doteki),
       meck:expect(doteki, get_env, 1, undefined),
       meck:expect(doteki, get_env, 2, []),
       meck:new(wok_sup),
       meck:expect(wok_sup, start_link, fun(X) -> X end)
   end,
   fun(_) ->
       meck:unload(wok_sup),
       meck:unload(doteki),
       meck:unload(bucs)
   end,
   [
    fun() ->
        ?assertEqual(#{}, wok_app:start(x, y))
    end
   ]}.

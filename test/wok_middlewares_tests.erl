-module(wok_middlewares_tests).
-export([routes/0, init/1]).

-include_lib("eunit/include/eunit.hrl").

% Fake middelware
routes() ->
  [
   {'GET', "/dummy_get", {?MODULE, my_dummy_get}},
   {'POST', "/dummy_post", {?MODULE, my_dummy_post}}
  ].

init(X) ->
  {ok, X}.

% Tests
wok_routes_no_ops_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{?MODULE,
                    []
                   }]
                 }]
               }])
   end,
   fun(_) -> ok end,
   [
    fun() ->
        ?assertMatch([{'POST',"/wok_middlewares_tests/dummy_post",
                       {wok_middlewares_tests,my_dummy_post}, wok_middlewares_tests},
                      {'GET',"/wok_middlewares_tests/dummy_get",
                       {wok_middlewares_tests,my_dummy_get}, wok_middlewares_tests}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_no_route_prefix_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{?MODULE,
                    [no_route_prefix]
                   }]
                 }]
               }])
   end,
   fun(_) -> ok end,
   [
    fun() ->
        ?assertMatch([{'POST',"/dummy_post",
                       {wok_middlewares_tests,my_dummy_post}, wok_middlewares_tests},
                      {'GET',"/dummy_get",
                       {wok_middlewares_tests,my_dummy_get}, wok_middlewares_tests}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_route_prefix_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{?MODULE,
                    [{route_prefix, "/custom"}]
                   }]
                 }]
               }])
   end,
   fun(_) -> ok end,
   [
    fun() ->
        ?assertMatch([{'POST',"/custom/dummy_post",
                       {wok_middlewares_tests,my_dummy_post}, wok_middlewares_tests},
                      {'GET',"/custom/dummy_get",
                       {wok_middlewares_tests,my_dummy_get}, wok_middlewares_tests}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_route_change_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{?MODULE,
                    [{route, "/dummy_get", "/get_dummy"}]
                   }]
                 }]
               }])
   end,
   fun(_) -> ok end,
   [
    fun() ->
        ?assertMatch([{'POST',"/wok_middlewares_tests/dummy_post",
                       {wok_middlewares_tests,my_dummy_post}, wok_middlewares_tests},
                      {'GET',"/wok_middlewares_tests/get_dummy",
                       {wok_middlewares_tests,my_dummy_get}, wok_middlewares_tests}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_route_change_all_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{?MODULE,
                    [
                     {route, "/dummy_get", "/get_dummy"},
                     {route, "/dummy_post", "/post_dummy"}
                    ]
                   }]
                 }]
               }])
   end,
   fun(_) -> ok end,
   [
    fun() ->
        ?assertMatch([{'POST',"/wok_middlewares_tests/post_dummy",
                       {wok_middlewares_tests,my_dummy_post}, wok_middlewares_tests},
                      {'GET',"/wok_middlewares_tests/get_dummy",
                       {wok_middlewares_tests,my_dummy_get}, wok_middlewares_tests}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_route_change_with_custom_prefix_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{?MODULE,
                    [
                     {route_prefix, "/custom"},
                     {route, "/dummy_get", "/get_dummy"}
                    ]
                   }]
                 }]
               }])
   end,
   fun(_) -> ok end,
   [
    fun() ->
        ?assertMatch([{'POST',"/custom/dummy_post",
                       {wok_middlewares_tests,my_dummy_post}, wok_middlewares_tests},
                      {'GET',"/custom/get_dummy",
                       {wok_middlewares_tests,my_dummy_get}, wok_middlewares_tests}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_route_change_with_no_prefix_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{?MODULE,
                    [
                     no_route_prefix,
                     {route, "/dummy_get", "/get_dummy"}
                    ]
                   }]
                 }]
               }])
   end,
   fun(_) -> ok end,
   [
    fun() ->
        ?assertMatch([{'POST',"/dummy_post",
                       {wok_middlewares_tests,my_dummy_post}, wok_middlewares_tests},
                      {'GET',"/get_dummy",
                       {wok_middlewares_tests,my_dummy_get}, wok_middlewares_tests}],
                     wok_middlewares:routes())
    end
   ]}.

wok_middelware_no_middleware_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config([{wok, [{middlewares, []}]}]),
       wok_middlewares:start_link()
   end,
   fun
     ({ok, _}) ->
       wok_middlewares:stop();
     (_) ->
       ok
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({ok, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(?MODULE)) end,
         fun(_) -> ?assertMatch({ok, message}, wok_middlewares:incoming_message(message)) end,
         fun(_) -> ?assertMatch({ok, message}, wok_middlewares:outgoing_message(message)) end]
       }
   end}.

wok_middelware_without_state_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [{?MODULE, []}]
                                          }]
                                        }]),
       wok_middlewares:start_link()
   end,
   fun
     ({ok, _}) ->
       wok_middlewares:stop();
     (_) ->
       ok
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({ok, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(?MODULE)) end]
       }
   end}.

wok_middelware_with_state_test_() ->
  {setup,
   fun() ->
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [{?MODULE,
                                             [
                                              {init, [init, parameters]}
                                             ]
                                            }]
                                          }]
                                        }]),
       wok_middlewares:start_link()
   end,
   fun
     ({ok, _}) ->
       wok_middlewares:stop();
     (_) ->
       ok
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({ok, _}, X) end,
         fun(_) -> ?assertMatch([init,parameters], wok_middlewares:state(?MODULE)) end,
         fun(_) -> ?assertMatch(ok, wok_middlewares:state(?MODULE, [init,new_parameters])) end,
         fun(_) -> ?assertMatch([init,new_parameters], wok_middlewares:state(?MODULE)) end]
       }
   end}.


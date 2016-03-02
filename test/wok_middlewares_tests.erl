-module(wok_middlewares_tests).

-include_lib("eunit/include/eunit.hrl").
-include("../include/wok.hrl").

meck_middleware() ->
  meck:new(fake_middleware, [non_strict]),
  meck:expect(fake_middleware, routes,
              fun() ->
                  [
                   {'GET', "/dummy_get", {fake_middleware, my_dummy_get}},
                   {'POST', "/dummy_post", {fake_middleware, my_dummy_post}}
                  ]
              end),
  meck:expect(fake_middleware, init,
              fun(X) ->
                  {ok, X}
              end).

unmeck_middleware() ->
  meck:unload(fake_middleware).

wok_r() ->
  #wok_req{local_state = [init,new_parameters]}.

% Tests
wok_routes_no_ops_test_() ->
  {setup,
   fun() ->
       meck_middleware(),
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{fake_middleware,
                    []
                   }]
                 }]
               }])
   end,
   fun(_) ->
       unmeck_middleware()
   end,
   [
    fun() ->
        ?assertMatch([{'POST',"/fake_middleware/dummy_post",
                       {fake_middleware,my_dummy_post}, fake_middleware},
                      {'GET',"/fake_middleware/dummy_get",
                       {fake_middleware,my_dummy_get}, fake_middleware}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_no_default_namespace_test_() ->
  {setup,
   fun() ->
       meck_middleware(),
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{fake_middleware,
                    [no_default_namespace]
                   }]
                 }]
               }])
   end,
   fun(_) ->
       unmeck_middleware()
   end,
   [
    fun() ->
        ?assertMatch([{'POST',"/dummy_post",
                       {fake_middleware,my_dummy_post}, fake_middleware},
                      {'GET',"/dummy_get",
                       {fake_middleware,my_dummy_get}, fake_middleware}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_namespace_test_() ->
  {setup,
   fun() ->
       meck_middleware(),
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{fake_middleware,
                    [{namespace, "/custom"}]
                   }]
                 }]
               }])
   end,
   fun(_) ->
       unmeck_middleware()
   end,
   [
    fun() ->
        ?assertMatch([{'POST',"/custom/dummy_post",
                       {fake_middleware,my_dummy_post}, fake_middleware},
                      {'GET',"/custom/dummy_get",
                       {fake_middleware,my_dummy_get}, fake_middleware}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_route_change_test_() ->
  {setup,
   fun() ->
       meck_middleware(),
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{fake_middleware,
                    [{route, "/dummy_get", "/get_dummy"}]
                   }]
                 }]
               }])
   end,
   fun(_) ->
       unmeck_middleware()
   end,
   [
    fun() ->
        ?assertMatch([{'POST',"/fake_middleware/dummy_post",
                       {fake_middleware,my_dummy_post}, fake_middleware},
                      {'GET',"/fake_middleware/get_dummy",
                       {fake_middleware,my_dummy_get}, fake_middleware}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_route_change_all_test_() ->
  {setup,
   fun() ->
       meck_middleware(),
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{fake_middleware,
                    [
                     {route, "/dummy_get", "/get_dummy"},
                     {route, "/dummy_post", "/post_dummy"}
                    ]
                   }]
                 }]
               }])
   end,
   fun(_) ->
       unmeck_middleware()
   end,
   [
    fun() ->
        ?assertMatch([{'POST',"/fake_middleware/post_dummy",
                       {fake_middleware,my_dummy_post}, fake_middleware},
                      {'GET',"/fake_middleware/get_dummy",
                       {fake_middleware,my_dummy_get}, fake_middleware}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_route_change_with_custom_prefix_test_() ->
  {setup,
   fun() ->
       meck_middleware(),
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{fake_middleware,
                    [
                     {namespace, "/custom"},
                     {route, "/dummy_get", "/get_dummy"}
                    ]
                   }]
                 }]
               }])
   end,
   fun(_) ->
       unmeck_middleware()
   end,
   [
    fun() ->
        ?assertMatch([{'POST',"/custom/dummy_post",
                       {fake_middleware,my_dummy_post}, fake_middleware},
                      {'GET',"/custom/get_dummy",
                       {fake_middleware,my_dummy_get}, fake_middleware}],
                     wok_middlewares:routes())
    end
   ]}.

wok_routes_route_change_with_no_prefix_test_() ->
  {setup,
   fun() ->
       meck_middleware(),
       ok = doteki:set_env_from_config(
              [{wok,
                [{middlewares,
                  [{fake_middleware,
                    [
                     no_default_namespace,
                     {route, "/dummy_get", "/get_dummy"}
                    ]
                   }]
                 }]
               }])
   end,
   fun(_) ->
       unmeck_middleware()
   end,
   [
    fun() ->
        ?assertMatch([{'POST',"/dummy_post",
                       {fake_middleware,my_dummy_post}, fake_middleware},
                      {'GET',"/get_dummy",
                       {fake_middleware,my_dummy_get}, fake_middleware}],
                     wok_middlewares:routes())
    end
   ]}.

wok_middelware_without_state_test_() ->
  {setup,
   fun() ->
       meck_middleware(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [{fake_middleware, []}]
                                          }]
                                        }]),
       wok_middlewares:start_link()
   end,
   fun
     ({ok, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware();
     (_) ->
       unmeck_middleware()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({ok, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware)) end]
       }
   end}.

wok_middelware_with_state_test_() ->
  {setup,
   fun() ->
       meck_middleware(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [{fake_middleware,
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
       wok_middlewares:stop(),
       unmeck_middleware();
     (_) ->
       unmeck_middleware()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({ok, _}, X) end,
         fun(_) -> ?assertMatch([init,parameters], wok_middlewares:state(fake_middleware)) end,
         fun(_) -> ?assertMatch(ok, wok_middlewares:state(fake_middleware, wok_r())) end,
         fun(_) -> ?assertMatch([init,new_parameters], wok_middlewares:state(fake_middleware)) end]
       }
   end}.


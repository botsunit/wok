-module(wok_cowboy_handler_tests).

-include_lib("eunit/include/eunit.hrl").

meck_rest_handler() ->
  meck:new(fake_rest_handler, [non_strict]),
  meck:expect(fake_rest_handler, create, fun(WokReq) -> WokReq end),
  meck:expect(fake_rest_handler, index, fun(WokReq) -> WokReq end),
  meck:expect(fake_rest_handler, show, fun(WokReq) -> WokReq end),
  meck:expect(fake_rest_handler, update, fun(WokReq) -> WokReq end).

unmeck_rest_handler() ->
  meck:unload(fake_rest_handler).

wok_cowboy_handler_uncompiled_routes_test_() ->
  {setup,
   fun() ->
    meck_rest_handler(),
    ok = doteki:set_env_from_config(
      [{wok,
        [{rest,
          [{routes, [
            {namespace, "/api", [
              {resources, users, fake_rest_handler}
            ]}
          ]}]
        }]
      }]
    )
   end,
   fun(_) -> unmeck_rest_handler() end,
   [fun() ->
        ?assertMatch(
          {
            [{
            "/api/users/:id", [
              {'PATCH', {fake_rest_handler, update}},
              {'PUT', {fake_rest_handler, update}},
              {'GET', {fake_rest_handler, show}}
            ]},{
            "/api/users", [
              {'POST', {fake_rest_handler, create}},
              {'GET', {fake_rest_handler, index}}]}
            ], #{static_path := [],static_route := []}
          },
           wok_http_handler:routes()
        )
    end]}.

wok_cowboy_handler_routes_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) -> ok end,
   [fun() ->
        ?assertMatch(
           {[{'_',[],[{
                [<<"route">>, <<"one">>],
                [],
                wok_cowboy_handler,
                [{'GET', {dummy_service_handler, my_service_get}}]
               }]}],
            #{static_path := [],static_route := []}},
           wok_cowboy_handler:routes([{'GET', "/route/one", {dummy_service_handler, my_service_get}}]))
    end,
    fun() ->
        ?assertMatch(
           {[{'_',[],[{
                [<<"route">>, <<"one">>],
                [],
                wok_cowboy_handler,
                [{'POST', {dummy_service_handler, my_service_get}}]
               }]}],
            #{static_path := [],static_route := []}},
           wok_cowboy_handler:routes([{'POST', "/route/one", {dummy_service_handler, my_service_get}}]))
    end,
    fun() ->
        ?assertMatch(
           {[{'_',[],[{
                [<<"route">>, <<"one">>],
                [],
                wok_cowboy_handler,
                [{'CUSTOM', {dummy_service_handler, my_service_get}}]
               }]}],
            #{static_path := [],static_route := []}},
           wok_cowboy_handler:routes([{'CUSTOM', "/route/one", {dummy_service_handler, my_service_get}}]))
    end,
    fun() ->
        ?assertMatch(
           {[{'_',[],[{
                [<<"priv">>, <<"public">>, '...'],
                [],
                cowboy_static,
                {dir, "/tmp", [{mimetypes,cow_mimetypes,all}, {default_file, "index.html"}]}
               }]}],
            #{static_path := "/tmp",static_route := "/priv/public"}},
           wok_cowboy_handler:routes([{static, "/priv/public", {dir, "/tmp"}}]))
    end]}.

wok_cowboy_handler_CORS_default_test_() ->
  {setup,
   fun() ->
       doteki:set_env_from_config(
         [{wok,
           [
            {rest, [
                    {port, 8080},
                    {ip, "0.0.0.0"},
                    {max_conn, 100},
                    {routes, [
                              {'GET', "/test", {dummy_service_handler, test_get}},
                              {'POST', "/test", {dummy_service_handler, test_port}},
                              {'PUT', "/test", {dummy_service_handler, test_put}},
                              {'DELETE', "/test", {dummy_service_handler, test_delete}},
                              {'DUMMY', "/test", {dummy_service_handler, test_dummy}},
                              {'PATCH', "/other/test", {dummy_service_handler, other_test_patch}},
                              {'HEAD', "/other/test", {dummy_service_handler, other_test_head}},
                              {'WS', "/test", dummy_service_handler},
                              {static, "/public", {priv_dir, wok, "static"}}
                             ]}
                   ]}
           ]}])
   end,
   fun(X) ->
       X
   end,
   [fun() ->
        CORS = wok_http_handler:cors_headers(<<"/test">>),
        ?assertMatch(<<"DELETE, PUT, POST, GET, OPTIONS">>,
                     buclists:keyfind(<<"Access-Control-Allow-Methods">>, 1, CORS)),
        ?assertMatch(<<"1728000">>,
                     buclists:keyfind(<<"Access-Control-Max-Age">>, 1, CORS)),
        ?assertMatch(<<"Access-Control-Allow-Origin, ",
                       "Authorization, ",
                       "Origin, ",
                       "x-requested-with, ",
                       "Content-Type, ",
                       "Content-Range, ",
                       "Content-Disposition, ",
                       "Content-Description">>,
                     buclists:keyfind(<<"Access-Control-Allow-Headers">>, 1, CORS))
    end,
    fun() ->
        CORS = wok_http_handler:add_access_control_allow_origin(wok_http_handler:cors_headers(<<"/test">>)),
        ?assertMatch(<<"DELETE, PUT, POST, GET, OPTIONS">>,
                     buclists:keyfind(<<"Access-Control-Allow-Methods">>, 1, CORS)),
        ?assertMatch(<<"1728000">>,
                     buclists:keyfind(<<"Access-Control-Max-Age">>, 1, CORS)),
        ?assertMatch(<<"Access-Control-Allow-Origin, ",
                       "Authorization, ",
                       "Origin, ",
                       "x-requested-with, ",
                       "Content-Type, ",
                       "Content-Range, ",
                       "Content-Disposition, ",
                       "Content-Description">>,
                     buclists:keyfind(<<"Access-Control-Allow-Headers">>, 1, CORS)),
        ?assertMatch(<<"*">>,
                     buclists:keyfind(<<"Access-Control-Allow-Origin">>, 1, CORS))
    end,
    fun() ->
        CORS = wok_http_handler:add_access_control_allow_credentials(wok_http_handler:cors_headers(<<"/test">>)),
        ?assertMatch(<<"DELETE, PUT, POST, GET, OPTIONS">>,
                     buclists:keyfind(<<"Access-Control-Allow-Methods">>, 1, CORS)),
        ?assertMatch(<<"1728000">>,
                     buclists:keyfind(<<"Access-Control-Max-Age">>, 1, CORS)),
        ?assertMatch(<<"Access-Control-Allow-Origin, ",
                       "Authorization, ",
                       "Origin, ",
                       "x-requested-with, ",
                       "Content-Type, ",
                       "Content-Range, ",
                       "Content-Disposition, ",
                       "Content-Description">>,
                     buclists:keyfind(<<"Access-Control-Allow-Headers">>, 1, CORS)),
        ?assertMatch(<<"false">>,
                     buclists:keyfind(<<"Access-Control-Allow-Credentials">>, 1, CORS))
    end]}.


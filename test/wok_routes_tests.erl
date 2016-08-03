-module(wok_routes_tests).

-include_lib("eunit/include/eunit.hrl").

meck_rest_handler() ->
  meck:new(fake_rest_handler, [non_strict]),
  meck:expect(fake_rest_handler, create, fun(WokReq) -> WokReq end),
  meck:expect(fake_rest_handler, index, fun(WokReq) -> WokReq end),
  meck:expect(fake_rest_handler, show, fun(WokReq) -> WokReq end),
  meck:expect(fake_rest_handler, update, fun(WokReq) -> WokReq end),
  meck:expect(fake_rest_handler, get, fun(WokReq) -> WokReq end),
  meck:expect(fake_rest_handler, about, fun(WokReq) -> WokReq end),
  meck:expect(fake_rest_handler, chat, fun(WokReq) -> WokReq end).

unmeck_rest_handler() ->
  meck:unload(fake_rest_handler).

routes_test_() ->
  {setup,
   fun() ->
    meck_rest_handler(),
    ok = doteki:set_env_from_config(
      [{wok,
        [{rest,
          [{routes, [
            {namespace, "/api", [
              {resources, users, fake_rest_handler},
              {'GET', "/get", {fake_rest_handler, get}}
            ]},
            {'GET', "/about", {fake_rest_handler, about}},
            {'POST', "/chat/:id/private/:idroom", {fake_rest_handler, chat}},
            {static, "/public", {priv_dir, wok, "static"}}
          ]}]
        }]
      }]
    )
   end,
   fun(_) -> unmeck_rest_handler() end,
   [fun() ->
        ?assertMatch(
           {
            [
             {
              "/public/[...]", cowboy_static,
              {dir, StaticPath,
               [{mimetypes, cow_mimetypes, all}, {default_file, "index.html"}]}
             },
             {
              "/chat/:id/private/:idroom", [
                                            {'POST', {fake_rest_handler, chat}}
                                           ]},
             {
              "/api/users/:id", [
                                 {'PATCH', {fake_rest_handler, update}},
                                 {'PUT', {fake_rest_handler, update}},
                                 {'GET', {fake_rest_handler, show}}
                                ]},
             {
              "/api/users", [
                             {'POST', {fake_rest_handler, create}},
                             {'GET', {fake_rest_handler, index}}
                            ]},
             {
              "/api/get", [
                           {'GET', {fake_rest_handler, get}}
                          ]},
             {
              "/about", [
                         {'GET', {fake_rest_handler, about}}
                        ]}
            ],
            #{static_path := StaticPath, static_route := "/public"}
           },
           wok_http_handler:routes()
        )
    end
    , fun() ->
          ?assertMatch("/api/get", wok_routes:path(fake_rest_handler, get)),
          ?assertMatch("/api/users/:id", wok_routes:path(fake_rest_handler, show)),
          ?assertMatch("/api/users/:id", wok_routes:path('PATCH', fake_rest_handler, update)),
          ?assertMatch("/api/users/:id", wok_routes:path('PUT', fake_rest_handler, update)),
          ?assertMatch("/api/users/123", wok_routes:path('PUT', fake_rest_handler, update, #{id => 123})),
          ?assertMatch("/chat/123/private/456", wok_routes:path('POST', fake_rest_handler, chat, #{id => 123, idroom => 456}))
      end
   ]}.


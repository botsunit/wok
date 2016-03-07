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

wok_rest_handler_uncompiled_routes_test_() ->
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
            [{
            "/api/users/:id", wok_rest_handler, [
              {'PATCH', {fake_rest_handler, update}},
              {'PUT', {fake_rest_handler, update}},
              {'GET', {fake_rest_handler, show}}
            ]},{
            "/api/users", wok_rest_handler, [
              {'POST', {fake_rest_handler, create}},
              {'GET', {fake_rest_handler, index}}
            ]},{
            "/api/get", wok_rest_handler, [
              {'GET', {fake_rest_handler, get}}
            ]},{
            "/about", wok_rest_handler, [
              {'GET', {fake_rest_handler, about}}
            ]},{
            "/chat/:id/private/:idroom", wok_rest_handler, [
              {'POST', {fake_rest_handler, chat}}
            ]},{
            "/public/[...]",cowboy_static,
              {dir,StaticPath,
                [{mimetypes,cow_mimetypes,all},{default_file,"index.html"}]}
            }
            ], #{static_path := StaticPath, static_route := "/public"}
          },
           wok_rest_handler:wok_routes()
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


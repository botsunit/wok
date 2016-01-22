-module(wok_rest_handler_tests).

-include_lib("eunit/include/eunit.hrl").

wok_rest_handler_routes_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) -> ok end,
   [fun() ->
        ?assertMatch(
           [{'_',[],[{
               [<<"route">>, <<"one">>],
               [],
               wok_rest_handler,
               [{'GET', {dummy_service_handler, my_service_get}}]
              }]}],
           wok_rest_handler:routes([{'GET', "/route/one", {dummy_service_handler, my_service_get}}]))
    end,
    fun() ->
        ?assertMatch(
           [{'_',[],[{
               [<<"route">>, <<"one">>],
               [],
               wok_rest_handler,
               [{'POST', {dummy_service_handler, my_service_get}}]
              }]}],
           wok_rest_handler:routes([{'POST', "/route/one", {dummy_service_handler, my_service_get}}]))
    end,
    fun() ->
        ?assertMatch(
           [{'_',[],[{
               [<<"route">>, <<"one">>],
               [],
               wok_rest_handler,
               [{'CUSTOM', {dummy_service_handler, my_service_get}}]
              }]}],
           wok_rest_handler:routes([{'CUSTOM', "/route/one", {dummy_service_handler, my_service_get}}]))
    end,
    fun() ->
        ?assertMatch(
           [{'_',[],[{
               [<<"priv">>, <<"public">>, '...'],
               [],
               cowboy_static,
               {dir, "/tmp", [{mimetypes,cow_mimetypes,all}, {default_file, "index.html"}]}
              }]}],
           wok_rest_handler:routes([{static, "/priv/public", {dir, "/tmp"}}]))
    end]}.

wok_rest_handler_CORS_default_test_() ->
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
        CORS = wok_rest_handler:cors_headers(<<"/test">>),
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
        CORS = wok_rest_handler:add_access_control_allow_origin(wok_rest_handler:cors_headers(<<"/test">>)),
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
    end]}.


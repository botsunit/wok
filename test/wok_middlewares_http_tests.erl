-module(wok_middlewares_http_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").
-include_lib("wok_http_adapter/include/wok_req.hrl").
-include("../include/wok.hrl").

meck_middleware_one() ->
  meck:new(fake_middleware_one, [non_strict]),
  meck:expect(fake_middleware_one, init,
              fun(X) ->
                  {ok, X}
              end),
  meck:expect(fake_middleware_one, incoming_http,
              fun(R) ->
                  {continue, R}
              end),
  meck:expect(fake_middleware_one, outgoing_http,
              fun(WokReq) ->
                wok_response:set_headers(WokReq, [{<<"Content-Type">>, <<"text/plain">>}])
              end).

unmeck_middleware_one() ->
  meck:unload(fake_middleware_one).

meck_middleware_two() ->
  meck:new(fake_middleware_two, [non_strict]),
  meck:expect(fake_middleware_two, init,
              fun(X) ->
                  {ok, X}
              end),
  meck:expect(fake_middleware_two, incoming_http,
              fun(R) ->
                  {continue, R}
            end),
  meck:expect(fake_middleware_two, outgoing_http,
              fun(WokReq) ->
                wok_response:set_headers(WokReq, [{<<"Content-Type">>, <<"text/html">>}])
              end).

unmeck_middleware_two() ->
  meck:unload(fake_middleware_two).

meck_middleware_three() ->
  meck:new(fake_middleware_three, [non_strict]),
  meck:expect(fake_middleware_three, init,
              fun(X) ->
                  {ok, X}
              end),
  meck:expect(fake_middleware_three, incoming_http,
              fun(WokReq) ->
                  {continue, WokReq}
              end),
  meck:expect(fake_middleware_three, outgoing_http,
              fun(WokReq) ->
                wok_response:set_headers(WokReq, [{<<"Content-Type">>, <<"text/html">>}])
              end).

unmeck_middleware_three() ->
  meck:unload(fake_middleware_three).

meck_middleware_four() ->
  meck:new(fake_middleware_four, [non_strict]).

unmeck_middleware_four() ->
  meck:unload(fake_middleware_four).

wok_r() ->
  #wok_req{
    adapter = wok_cowboy_req,
    request = cowboy_req:new(
      undefined, % Socket
      dummy_transport, % Transport
      undefined, % Peer
      <<"GET">>, % Method
      <<"/test/path">>, % Path
      <<"?a=b&c=d">>, % Query
      'HTTP/1.1', % Version
      [{<<"X-Wok-Test">>, <<"true">>}], % Headers
      <<"botsunit.com">>, % Host
      8080, % Port
      <<>>, % Buffer
      true, % CanKeepalive
      true, % Compress
      undefined % OnResponse
    )}.

%% Tests

wok_middelware_no_middleware_test_() ->
  {setup,
   fun() ->
       meck_middleware_one(),
       ok = doteki:set_env_from_config([{wok, [{middlewares, []}]}]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_one();
     (_) ->
       unmeck_middleware_one()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_one)) end,
         fun({_, Req}) ->
             ?assertMatch({continue, Req}, wok_middlewares:incoming_http(Req))
         end,
         fun({_, Req}) -> ?assertMatch(Req,
                                       wok_middlewares:outgoing_http(Req)) end]
       }
   end}.

wok_middelware_inout_two_test_() ->
  {setup,
   fun() ->
       meck_middleware_two(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_two, []}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_two();
     (_) ->
       unmeck_middleware_two()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_two)) end,
         fun({_, Req}) -> ?assertMatch({continue, Req}, wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) ->
             ?assertContinueIfMatch(Req2 = #wok_req{},
                                    wok_middlewares:outgoing_http(Req),
                                    Req2,
                                    fun(ReqCont) ->
                                        ?assertMatch(
                                           [{<<"Content-Type">>, <<"text/html">>}],
                                           wok_req:get_response_headers(ReqCont))
                                    end)
         end]
       }
   end}.

wok_middelware_one_and_two_test_() ->
  {setup,
   fun() ->
       meck_middleware_one(),
       meck_middleware_two(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_one, []},
                                            {fake_middleware_two, []}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_one(),
       unmeck_middleware_two();
     (_) ->
       unmeck_middleware_one(),
       unmeck_middleware_two()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_one)) end,
         fun({_, Req}) -> ?assertMatch({continue, #wok_req{}}, wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) ->
             ?assertContinueIfMatch(Req2 = #wok_req{},
                                    wok_middlewares:outgoing_http(Req),
                                    Req2,
                                    fun(ReqCont) ->
                                        ?assertMatch(
                                           [{<<"Content-Type">>, <<"text/plain">>}],
                                           wok_req:get_response_headers(ReqCont))
                                    end)
         end]
       }
   end}.

wok_middelware_stop_test_() ->
  {setup,
   fun() ->
       meck_middleware_one(),
       meck_middleware_three(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_one, []},
                                            {fake_middleware_three, []}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_one(),
       unmeck_middleware_three();
     (_) ->
       unmeck_middleware_one(),
       unmeck_middleware_three()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_one)) end,
         fun({_, Req}) -> ?assertMatch({continue, #wok_req{}},
                                       wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) ->
             ?assertContinueIfMatch(Req2 = #wok_req{},
                                    wok_middlewares:outgoing_http(Req),
                                    Req2,
                                    fun(ReqCont) ->
                                        ?assertMatch(
                                           [{<<"Content-Type">>, <<"text/plain">>}],
                                           wok_req:get_response_headers(ReqCont))
                                    end)
         end]
       }
   end}.

wok_middelware_one_and_two_with_only_two_test_() ->
  {setup,
   fun() ->
       meck_middleware_one(),
       meck_middleware_two(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_one,
                                             [
                                              {http,
                                               [
                                                {only, ["/only"]}
                                               ]}
                                             ]},
                                            {fake_middleware_two, []}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_one(),
       unmeck_middleware_two();
     (_) ->
       unmeck_middleware_one(),
       unmeck_middleware_two()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_one)) end,
         fun({_, Req}) -> ?assertMatch({continue, #wok_req{}}, wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) ->
             ?assertContinueIfMatch(Req2 = #wok_req{},
                                    wok_middlewares:outgoing_http(Req),
                                    Req2,
                                    fun(ReqCont) ->
                                        ?assertMatch(
                                           [{<<"Content-Type">>, <<"text/html">>}],
                                           wok_req:get_response_headers(ReqCont))
                                    end)
         end]
       }
   end}.

wok_middelware_one_and_two_with_only_one_test_() ->
  {setup,
   fun() ->
       meck_middleware_one(),
       meck_middleware_two(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_one, []},
                                            {fake_middleware_two,
                                             [
                                              {http,
                                               [
                                                {only, ["/only"]}
                                               ]}
                                             ]}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_one(),
       unmeck_middleware_two();
     (_) ->
       unmeck_middleware_one(),
       unmeck_middleware_two()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_one)) end,
         fun({_, Req}) -> ?assertMatch({continue, #wok_req{}}, wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) ->
             ?assertContinueIfMatch(Req2 = #wok_req{},
                                    wok_middlewares:outgoing_http(Req),
                                    Req2,
                                    fun(ReqCont) ->
                                        ?assertMatch(
                                           [{<<"Content-Type">>, <<"text/plain">>}],
                                           wok_req:get_response_headers(ReqCont))
                                    end)
         end]
       }
   end}.

wok_middelware_one_and_two_with_except_two_test_() ->
  {setup,
   fun() ->
       meck_middleware_one(),
       meck_middleware_two(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_one, []},
                                            {fake_middleware_two,
                                             [
                                              {http,
                                               [
                                                {except, ["/*/path"]}
                                               ]}
                                             ]}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_one(),
       unmeck_middleware_two();
     (_) ->
       unmeck_middleware_one(),
       unmeck_middleware_two()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_one)) end,
         fun({_, Req}) -> ?assertMatch({continue, #wok_req{}}, wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) ->
             ?assertContinueIfMatch(Req2 = #wok_req{},
                                    wok_middlewares:outgoing_http(Req),
                                    Req2,
                                    fun(ReqCont) ->
                                        ?assertMatch(
                                           [{<<"Content-Type">>, <<"text/plain">>}],
                                           wok_req:get_response_headers(ReqCont))
                                    end)
         end]
       }
   end}.

wok_middelware_one_and_two_with_except_one_test_() ->
  {setup,
   fun() ->
       meck_middleware_one(),
       meck_middleware_two(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_one,
                                             [
                                              {http,
                                               [
                                                {except, ["/*/path"]}
                                               ]}
                                             ]},
                                            {fake_middleware_two, []}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_one(),
       unmeck_middleware_two();
     (_) ->
       unmeck_middleware_one(),
       unmeck_middleware_two()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_one)) end,
         fun({_, Req}) -> ?assertMatch({continue, #wok_req{}}, wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) ->
             ?assertContinueIfMatch(Req2 = #wok_req{},
                                    wok_middlewares:outgoing_http(Req),
                                    Req2,
                                    fun(ReqCont) ->
                                        ?assertMatch(
                                           [{<<"Content-Type">>, <<"text/html">>}],
                                           wok_req:get_response_headers(ReqCont))
                                    end)
         end]
       }
   end}.

wok_middelware_one_and_two_with_except_one_with_get_test_() ->
  {setup,
   fun() ->
       meck_middleware_one(),
       meck_middleware_two(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_one,
                                             [
                                              {http,
                                               [
                                                {except, [{"/*/path", ['GET']}]}
                                               ]}
                                             ]},
                                            {fake_middleware_two,
                                             [
                                             ]}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_one(),
       unmeck_middleware_two();
     (_) ->
       unmeck_middleware_one(),
       unmeck_middleware_two()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_one)) end,
         fun({_, Req}) -> ?assertMatch({continue, #wok_req{}}, wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) ->
             ?assertContinueIfMatch(Req2 = #wok_req{},
                                    wok_middlewares:outgoing_http(Req),
                                    Req2,
                                    fun(ReqCont) ->
                                        ?assertMatch(
                                           [{<<"Content-Type">>, <<"text/html">>}],
                                           wok_req:get_response_headers(ReqCont))
                                    end)
         end]
       }
   end}.

wok_middelware_one_and_two_with_except_one_with_post_test_() ->
  {setup,
   fun() ->
       meck_middleware_one(),
       meck_middleware_two(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_one,
                                             [
                                              {http,
                                               [
                                                {except, [{"/*/path", ['POST']}]}
                                               ]}
                                             ]},
                                            {fake_middleware_two, []}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_one(),
       unmeck_middleware_two();
     (_) ->
       unmeck_middleware_one(),
       unmeck_middleware_two()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_one)) end,
         fun({_, Req}) -> ?assertMatch({continue, #wok_req{}}, wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) ->
             ?assertContinueIfMatch(Req2 = #wok_req{},
                                    wok_middlewares:outgoing_http(Req),
                                    Req2,
                                    fun(ReqCont) ->
                                        ?assertMatch(
                                           [{<<"Content-Type">>, <<"text/plain">>}],
                                           wok_req:get_response_headers(ReqCont))
                                    end)
         end]
       }
   end}.

wok_middelware_one_and_two_with_only_one_with_get_test_() ->
  {setup,
   fun() ->
       meck_middleware_one(),
       meck_middleware_two(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_one,
                                             [
                                              {http,
                                               [
                                                {only, [{"/*/path", ['GET']}]}
                                               ]}
                                             ]},
                                            {fake_middleware_two,
                                             [
                                             ]}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_one(),
       unmeck_middleware_two();
     (_) ->
       unmeck_middleware_one(),
       unmeck_middleware_two()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_one)) end,
         fun({_, Req}) -> ?assertMatch({continue, #wok_req{}}, wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) ->
             ?assertContinueIfMatch(Req2 = #wok_req{},
                                    wok_middlewares:outgoing_http(Req),
                                    Req2,
                                    fun(ReqCont) ->
                                        ?assertMatch(
                                           [{<<"Content-Type">>, <<"text/plain">>}],
                                           wok_req:get_response_headers(ReqCont))
                                    end)
         end]
       }
   end}.

wok_middelware_one_and_two_with_only_one_with_post_test_() ->
  {setup,
   fun() ->
       meck_middleware_one(),
       meck_middleware_two(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_one,
                                             [
                                              {http,
                                               [
                                                {only, [{"/*/path", ['POST']}]}
                                               ]}
                                             ]},
                                            {fake_middleware_two, []}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_one(),
       unmeck_middleware_two();
     (_) ->
       unmeck_middleware_one(),
       unmeck_middleware_two()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_one)) end,
         fun({_, Req}) -> ?assertMatch({continue, #wok_req{}}, wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) ->
             ?assertContinueIfMatch(Req2 = #wok_req{},
                                    wok_middlewares:outgoing_http(Req),
                                    Req2,
                                    fun(ReqCont) ->
                                        ?assertMatch(
                                           [{<<"Content-Type">>, <<"text/html">>}],
                                           wok_req:get_response_headers(ReqCont))
                                    end)
         end]
       }
   end}.

wok_middelware_with_missing_methods_test_() ->
  {setup,
   fun() ->
       meck_middleware_four(),
       ok = doteki:set_env_from_config([{wok,
                                         [{middlewares,
                                           [
                                            {fake_middleware_four, []}
                                           ]
                                          }]
                                        }]),
       {wok_middlewares:start_link(), wok_r()}
   end,
   fun
     ({{ok, _}, _}) ->
       wok_middlewares:stop(),
       unmeck_middleware_four();
     (_) ->
       unmeck_middleware_four()
   end,
   fun(R) ->
       {with, R,
        [fun(X) -> ?assertMatch({{ok, _}, _}, X) end,
         fun(_) -> ?assertMatch(nostate, wok_middlewares:state(fake_middleware_four)) end,
         fun({_, Req}) -> ?assertMatch({continue, #wok_req{}}, wok_middlewares:incoming_http(Req)) end,
         fun({_, Req}) -> ?assertMatch(Req, wok_middlewares:outgoing_http(Req)) end]
       }
   end}.


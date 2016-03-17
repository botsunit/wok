-module(wok_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").

wok_request_custom_data_test_() ->
  {setup,
   fun() ->
       wok_test_req:new('GET', "/fake", [], <<>>, [], [], [])
   end,
   fun(_) -> ok end,
   fun(R) ->
       {with, R,
        [
         fun(Req) ->
             ?assertContinueIfMatch(
                {ok, Req2}, wok_request:custom_data(Req, hello, "World"), Req2,
                fun(Req3) ->
                    ?assertContinueIfMatch(
                       {ok, "World", Req4}, wok_request:custom_data(Req3, hello, "Monde"), Req4,
                       fun(Req5) ->
                           ?assertMatch("Monde", wok_request:custom_data(Req5, hello))
                       end)
                end)
         end
        ]}
   end}.


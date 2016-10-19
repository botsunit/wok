-module(wok_request_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").

% TODO wok_request_custom_data_test_() ->
% TODO   {setup,
% TODO    fun() ->
% TODO        wok_test_req:new('GET', "/fake", [], <<>>, [], [], [])
% TODO    end,
% TODO    fun(_) -> ok end,
% TODO    fun(R) ->
% TODO        {with, R,
% TODO         [
% TODO          fun(Req) ->
% TODO              ?assertContinueIfMatch(
% TODO                 {ok, Req2}, wok_request:custom_data(Req, hello, "World"), Req2,
% TODO                 fun(Req3) ->
% TODO                     ?assertContinueIfMatch(
% TODO                        {ok, "World", Req4}, wok_request:custom_data(Req3, hello, "Monde"), Req4,
% TODO                        fun(Req5) ->
% TODO                            ?assertMatch("Monde", wok_request:custom_data(Req5, hello))
% TODO                        end)
% TODO                 end)
% TODO          end
% TODO         ]}
% TODO    end}.


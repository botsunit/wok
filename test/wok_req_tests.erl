-module(wok_req_tests).
-include("../include/wok.hrl").

-include_lib("eunit/include/eunit.hrl").

wok_req_test_() ->
  {setup,
   fun() ->
     meck:new(cowboy_req, [non_strict, passthrough]),
     meck:expect(cowboy_req, undefined_function, fun(R) ->
      element(1, R) =:= http_req
     end)
   end,
   fun(_) ->
     meck:unload(cowboy_req)
   end,
   [
     fun() ->
        R = #wok_req{request = undefined,
                     custom_data = "initial_custom_data"},
        ?assertEqual("initial_custom_data", wok_req:get_custom_data(R)),
        R2 = wok_req:set_custom_data(R, "new_custom_data"),
        ?assert(is_record(R2, wok_req)),
        ?assertEqual("new_custom_data", wok_req:get_custom_data(R2))
     end
   ]}.

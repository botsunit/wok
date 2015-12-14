-module(wok_rest_handler_tests).

-include_lib("eunit/include/eunit.hrl").

wok_test_() ->
  {setup,
   fun setup/0,
   fun teardown/1,
   [
      ?_test(t_t())
   ]}.

setup() ->
  ok.

teardown(_) ->
  ok.

t_t() ->
  ?assertMatch(
     [{'_',[],[{
         [<<"route">>, <<"one">>],
         [],
         wok_rest_handler,
         [{'GET', {dummy_service_handler, my_service_get}}]
        }]}],
       wok_rest_handler:routes([{'GET', "/route/one", {dummy_service_handler, my_service_get}}])),
  ?assertMatch(
     [{'_',[],[{
         [<<"route">>, <<"one">>],
         [],
         wok_rest_handler,
         [{'POST', {dummy_service_handler, my_service_get}}]
        }]}],
       wok_rest_handler:routes([{'POST', "/route/one", {dummy_service_handler, my_service_get}}])),
  ?assertMatch(
     [{'_',[],[{
         [<<"route">>, <<"one">>],
         [],
         wok_rest_handler,
         [{'CUSTOM', {dummy_service_handler, my_service_get}}]
        }]}],
       wok_rest_handler:routes([{'CUSTOM', "/route/one", {dummy_service_handler, my_service_get}}])),
  ?assertMatch(
     [{'_',[],[{
         [<<"priv">>, <<"public">>, '...'],
         [],
         cowboy_static,
         {dir, "/tmp", [{mimetypes,cow_mimetypes,all}]}
        }]}],
     wok_rest_handler:routes([{static, "/priv/public", {dir, "/tmp"}}])).


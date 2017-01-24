-module(wok_producer_tests).

-include_lib("eunit/include/eunit.hrl").

meck_kafe() ->
  meck:new(kafe),
  meck:expect(kafe, produce,
              fun(Topic, Message) ->
                  <<Topic/binary, "::", Message/binary>>
              end),
  meck:expect(kafe, produce,
              fun([{Topic, [Message]}]) ->
                  <<Topic/binary, "::", Message/binary>>
              end).

unmeck_kafe() ->
  meck: unload(kafe).

meck_custom_handler() ->
  meck:new(custom_handler, [non_strict]),
  meck:expect(custom_handler, create,
              fun(_From, _To, _Body, _Options) ->
                  <<"wok_producer/handler">>
              end).

unmeck_custom_handler() ->
  meck:unload(custom_handler).

meck_wok_handler() ->
  meck:new(wok_handler, [non_strict]),
  meck:expect(wok_handler, create,
              fun(_From, _To, _Body, _Options) ->
                  <<"wok/messages/handler">>
              end).

unmeck_wok_handler() ->
  meck:unload(wok_handler).

wok_producer_with_all_config_test_() ->
  {setup,
   fun() ->
       _ = meck_kafe(),
       _ = meck_custom_handler(),
       _ = meck_wok_handler(),
       _ = doteki:unset_env(wok),
       _ = doteki:unset_env(wok_producer),
       doteki:set_env_from_config(
         [
          {wok, [{messages, [{handler, wok_handler}]}]}
         ]
        )
   end,
   fun(_) ->
       _ = unmeck_kafe(),
       _ = unmeck_custom_handler(),
       _ = unmeck_wok_handler()
   end,
   [
    fun() ->
        ?assertMatch(<<"test::message">>,
                     wok_producer:provide(<<"test">>, <<"message">>))
    end
    , fun() ->
          ?assertMatch(<<"test::wok/messages/handler">>,
                       wok_producer:provide(<<"test">>, <<"from">>, <<"to">>, <<"body">>))
      end
    , fun() ->
          ?assertMatch(<<"test::wok/messages/handler">>,
                       wok_producer:provide(<<"test">>, <<"from">>, <<"to">>, <<"body">>, []))
      end
    , fun() ->
          ?assertMatch(<<"test::wok/messages/handler">>,
                       wok_producer:provide(<<"test">>, {<<"from">>, <<"to">>, <<"body">>}))
      end
    , fun() ->
          ?assertMatch(<<"test::wok/messages/handler">>,
                       wok_producer:provide(<<"test">>, {<<"from">>, <<"to">>, <<"body">>, []}))
      end
   ]}.

wok_producer_without_config_test_() ->
  {setup,
   fun() ->
       _ = meck_kafe(),
       _ = meck_custom_handler(),
       _ = meck_wok_handler(),
       _ = meck:new(uuid, [passthrough]),
       _ = meck:expect(uuid, to_string, fun(_) -> "76c22bf7-6a92-4a08-8c38-65206e4f51b2" end),
       _ = doteki:unset_env(wok),
       _ = doteki:unset_env(wok_producer),
       ok
   end,
   fun(_) ->
       _ = unmeck_kafe(),
       _ = unmeck_custom_handler(),
       _ = unmeck_wok_handler(),
       _ = meck:unload(uuid),
       ok
   end,
   [
    fun() ->
        ?assertMatch(<<"test::message">>,
                     wok_producer:provide(<<"test">>, <<"message">>))
    end
    , fun() ->
          ?assertMatch(
             <<116, 101, 115, 116, 58, 58, 1, 30, 11, 8, 1, 1, 11, 8, 2, 116, 111, 1, 11, 8, 4, 102, 114, 111, 109,
               1, 11, 8, 36, 55, 54, 99, 50, 50, 98, 102, 55, 45, 54, 97, 57, 50, 45, 52, 97, 48, 56, 45, 56, 99, 51,
               56, 45, 54, 53, 50, 48, 54, 101, 52, 102, 53, 49, 98, 50, 40, 11, 8, 2, 60, 11, 8, 11, 98, 105, 110,
               97, 114, 121, 95, 98, 111, 100, 121, 50, 1, 60, 11, 8, 8, 99, 111, 109, 112, 114, 101, 115, 115,
               50, 1, 44, 157, 183, 163, 219, 168, 11, 178, 1, 11, 8, 12, 120, 156, 75, 202, 79, 169, 4, 0, 4, 26,
               1, 175>>,
             wok_producer:provide(<<"test">>, <<"from">>, <<"to">>, <<"body">>))
      end
    , fun() ->
          ?assertMatch(
             <<116, 101, 115, 116, 58, 58, 1, 30, 11, 8, 1, 1, 11, 8, 2, 116, 111, 1, 11, 8, 4, 102, 114, 111, 109,
               1, 11, 8, 36, 55, 54, 99, 50, 50, 98, 102, 55, 45, 54, 97, 57, 50, 45, 52, 97, 48, 56, 45, 56, 99, 51,
               56, 45, 54, 53, 50, 48, 54, 101, 52, 102, 53, 49, 98, 50, 40, 11, 8, 2, 60, 11, 8, 11, 98, 105, 110,
               97, 114, 121, 95, 98, 111, 100, 121, 50, 1, 60, 11, 8, 8, 99, 111, 109, 112, 114, 101, 115, 115,
               50, 1, 44, 157, 183, 163, 219, 168, 11, 178, 1, 11, 8, 12, 120, 156, 75, 202, 79, 169, 4, 0, 4, 26,
               1, 175>>,
             wok_producer:provide(<<"test">>, <<"from">>, <<"to">>, <<"body">>, []))
      end
    , fun() ->
          ?assertMatch(
             <<116, 101, 115, 116, 58, 58, 1, 30, 11, 8, 1, 1, 11, 8, 2, 116, 111, 1, 11, 8, 4, 102, 114, 111, 109,
               1, 11, 8, 36, 55, 54, 99, 50, 50, 98, 102, 55, 45, 54, 97, 57, 50, 45, 52, 97, 48, 56, 45, 56, 99, 51,
               56, 45, 54, 53, 50, 48, 54, 101, 52, 102, 53, 49, 98, 50, 40, 11, 8, 2, 60, 11, 8, 11, 98, 105, 110,
               97, 114, 121, 95, 98, 111, 100, 121, 50, 1, 60, 11, 8, 8, 99, 111, 109, 112, 114, 101, 115, 115,
               50, 1, 44, 157, 183, 163, 219, 168, 11, 178, 1, 11, 8, 12, 120, 156, 75, 202, 79, 169, 4, 0, 4, 26,
               1, 175>>,
             wok_producer:provide(<<"test">>, {<<"from">>, <<"to">>, <<"body">>}))
      end
    , fun() ->
          ?assertMatch(
             <<116, 101, 115, 116, 58, 58, 1, 30, 11, 8, 1, 1, 11, 8, 2, 116, 111, 1, 11, 8, 4, 102, 114, 111, 109,
               1, 11, 8, 36, 55, 54, 99, 50, 50, 98, 102, 55, 45, 54, 97, 57, 50, 45, 52, 97, 48, 56, 45, 56, 99, 51,
               56, 45, 54, 53, 50, 48, 54, 101, 52, 102, 53, 49, 98, 50, 40, 11, 8, 2, 60, 11, 8, 11, 98, 105, 110,
               97, 114, 121, 95, 98, 111, 100, 121, 50, 1, 60, 11, 8, 8, 99, 111, 109, 112, 114, 101, 115, 115,
               50, 1, 44, 157, 183, 163, 219, 168, 11, 178, 1, 11, 8, 12, 120, 156, 75, 202, 79, 169, 4, 0, 4, 26,
               1, 175>>,
             wok_producer:provide(<<"test">>, {<<"from">>, <<"to">>, <<"body">>, []}))
      end
   ]}.


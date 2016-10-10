-module(wok_utils_tests).

-include_lib("eunit/include/eunit.hrl").

wok_utils_consumer_group_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) -> ok end,
   [
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, 2, undefined),
        ?assertEqual(undefined, wok_utils:consumer_group()),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, 2, <<"MyConsumerGroup">>),
        ?assertEqual(<<"MyConsumerGroup">>, wok_utils:consumer_group()),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, 2, random),
        CG = wok_utils:consumer_group(),
        ?assertNotEqual(undefined, CG),
        ?assert(is_binary(CG)),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, 2, random),
        CG1 = wok_utils:consumer_group(),
        CG2 = wok_utils:consumer_group(),
        ?assertEqual(CG1, CG2),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, 2, {random}),
        CG = wok_utils:consumer_group(),
        ?assertNotEqual(undefined, CG),
        ?assert(is_binary(CG)),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, 2, {random}),
        CG1 = wok_utils:consumer_group(),
        CG2 = wok_utils:consumer_group(),
        ?assertEqual(CG1, CG2),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, 2, {random, [{prefix, prefix}]}),
        ?assertMatch(<<"prefix_", _/binary>>, wok_utils:consumer_group()),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, 2, {random, [{prefix, prefix}]}),
        CG1 = wok_utils:consumer_group(),
        CG2 = wok_utils:consumer_group(),
        ?assertEqual(CG1, CG2),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end
   ]}.

wok_utils_local_consumer_group_test_() ->
  {setup,
   fun() -> ok end,
   fun(_) -> ok end,
   [
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, fun
                                       ([wok, messages, consumer_group], _) ->
                                         undefined;
                                       ([wok, messages, local_consumer_group], X) ->
                                         X
                                     end),
        ?assertEqual(undefined, wok_utils:local_consumer_group()),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, fun
                                       ([wok, messages, consumer_group], _) ->
                                         <<"MyConsumerGroup">>;
                                       ([wok, messages, local_consumer_group], X) ->
                                         X
                                     end),
        ?assertEqual(<<"MyConsumerGroup">>, wok_utils:local_consumer_group()),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, fun
                                       ([wok, messages, consumer_group], _) ->
                                         random;
                                       ([wok, messages, local_consumer_group], X) ->
                                         X
                                     end),
        CG = wok_utils:local_consumer_group(),
        ?assertNotEqual(undefined, CG),
        ?assert(is_binary(CG)),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, fun
                                       ([wok, messages, consumer_group], _) ->
                                         random;
                                       ([wok, messages, local_consumer_group], X) ->
                                         X
                                     end),
        CG1 = wok_utils:local_consumer_group(),
        CG2 = wok_utils:local_consumer_group(),
        ?assertEqual(CG1, CG2),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, fun
                                       ([wok, messages, consumer_group], _) ->
                                         {random};
                                       ([wok, messages, local_consumer_group], X) ->
                                         X
                                     end),
        CG = wok_utils:local_consumer_group(),
        ?assertNotEqual(undefined, CG),
        ?assert(is_binary(CG)),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, fun
                                       ([wok, messages, consumer_group], _) ->
                                         {random};
                                       ([wok, messages, local_consumer_group], X) ->
                                         X
                                     end),
        CG1 = wok_utils:local_consumer_group(),
        CG2 = wok_utils:local_consumer_group(),
        ?assertEqual(CG1, CG2),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, fun
                                       ([wok, messages, consumer_group], _) ->
                                         {random, [{prefix, prefix}]};
                                       ([wok, messages, local_consumer_group], X) ->
                                         X
                                     end),
        ?assertMatch(<<"prefix_", _/binary>>, wok_utils:local_consumer_group()),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, fun
                                       ([wok, messages, consumer_group], _) ->
                                         {random, [{prefix, prefix}]};
                                       ([wok, messages, local_consumer_group], X) ->
                                         X
                                     end),
        CG1 = wok_utils:local_consumer_group(),
        CG2 = wok_utils:local_consumer_group(),
        ?assertEqual(CG1, CG2),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, fun
                                       ([wok, messages, consumer_group], _) ->
                                         {random, [{prefix, prefix}]};
                                       ([wok, messages, local_consumer_group], _) ->
                                         <<"MyLocalConsumerGroup">>
                                     end),
        ?assertEqual(<<"MyLocalConsumerGroup">>, wok_utils:local_consumer_group()),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end,
    fun() ->
        meck:new(doteki),
        meck:expect(doteki, get_env, fun
                                       ([wok, messages, consumer_group], _) ->
                                         {random, [{prefix, prefix}]};
                                       ([wok, messages, local_consumer_group], _) ->
                                         <<"MyLocalConsumerGroup">>
                                     end),
        CG1 = wok_utils:local_consumer_group(),
        ?assertEqual(<<"MyLocalConsumerGroup">>, CG1),
        CG2 = wok_utils:local_consumer_group(),
        ?assertEqual(CG1, CG2),
        meck:unload(doteki),
        ets:delete(wok_configuration)
    end
   ]}.

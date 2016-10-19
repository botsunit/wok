-module(wok_default_message_handler_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").
-include_lib("../include/wok_message_handler.hrl").

wok_default_message_handler_test_() ->
  {setup,
   fun() ->
       meck:new(uuid, [passthrough]),
       meck:expect(uuid, to_string, fun(_) -> "76c22bf7-6a92-4a08-8c38-65206e4f51b2" end),
       ok
   end,
   fun(_) ->
       meck:unload(uuid),
       ok
   end,
   [
    fun() ->
        Result = <<1, 30, 11, 8, 1, 1, 11, 8, 2, 116, 111, 1, 11, 8, 4, 102, 114, 111, 109, 1, 11, 8, 36, 55, 54, 99, 50,
                   50, 98, 102, 55, 45, 54, 97, 57, 50, 45, 52, 97, 48, 56, 45, 56, 99, 51, 56, 45, 54, 53, 50, 48, 54,
                   101, 52, 102, 53, 49, 98, 50, 40, 11, 8, 2, 60, 11, 8, 11, 98, 105, 110, 97, 114, 121, 95, 98, 111,
                   100, 121, 50, 1, 60, 11, 8, 8, 99, 111, 109, 112, 114, 101, 115, 115, 50, 1, 44, 157, 183, 163,
                   219, 168, 11, 178, 1, 11, 8, 12, 120, 156, 75, 202, 79, 169, 4, 0, 4, 26, 1, 175>>,
        Message = wok_default_message_handler:create(<<"from">>, <<"to">>, <<"body">>),
        ?assertEqual(Result, Message),
        ?assertMatch({ok, #msg{
                             uuid = <<"76c22bf7-6a92-4a08-8c38-65206e4f51b2">>,
                             to = [<<"to">>],
                             from = <<"from">>,
                             headers = #{compress := true,
                                         binary_body := true},
                             body = <<"body">>}, <<>>},
                     wok_default_message_handler:parse(Message))
    end,
    fun() ->
        Result = <<1, 30, 11, 8, 1, 1, 11, 8, 2, 116, 111, 1, 11, 8, 4, 102, 114, 111, 109, 1, 11, 8, 36, 55, 54, 99, 50,
                   50, 98, 102, 55, 45, 54, 97, 57, 50, 45, 52, 97, 48, 56, 45, 56, 99, 51, 56, 45, 54, 53, 50, 48, 54,
                   101, 52, 102, 53, 49, 98, 50, 40, 11, 8, 2, 60, 11, 8, 11, 98, 105, 110, 97, 114, 121, 95, 98, 111,
                   100, 121, 50, 1, 60, 11, 8, 8, 99, 111, 109, 112, 114, 101, 115, 115, 51, 0, 60, 150, 215, 161,
                   219, 168, 11, 178, 1, 11, 8, 4, 98, 111, 100, 121>>,
        Message = wok_default_message_handler:create(<<"from">>, <<"to">>, <<"body">>,
                                                     [{headers, #{compress => false}}]),
        ?assertEqual(Result, Message),
        ?assertMatch({ok, #msg{
                             uuid = <<"76c22bf7-6a92-4a08-8c38-65206e4f51b2">>,
                             to = [<<"to">>],
                             from = <<"from">>,
                             headers = #{compress := false,
                                         binary_body := true},
                             body = <<"body">>}, <<>>},
                     wok_default_message_handler:parse(Message))
    end,
    fun() ->
        Result = <<1, 30, 11, 8, 1, 1, 11, 8, 2, 116, 111, 1, 11, 8, 4, 102, 114, 111, 109, 1, 11, 8, 36, 55, 54, 99, 50,
                   50, 98, 102, 55, 45, 54, 97, 57, 50, 45, 52, 97, 48, 56, 45, 56, 99, 51, 56, 45, 54, 53, 50, 48, 54,
                   101, 52, 102, 53, 49, 98, 50, 40, 11, 8, 2, 60, 11, 8, 11, 98, 105, 110, 97, 114, 121, 95, 98, 111,
                   100, 121, 51, 0, 60, 11, 8, 8, 99, 111, 109, 112, 114, 101, 115, 115, 51, 0, 61, 0, 170, 182, 39,
                   139, 122, 40, 1, 11, 8, 31, 131, 104, 3, 109, 0, 0, 0, 4, 98, 111, 100, 121, 100, 0, 2, 97, 115, 108,
                   0, 0, 0, 1, 107, 0, 5, 116, 117, 112, 108, 101, 106>>,
        Message = wok_default_message_handler:create(<<"from">>, <<"to">>, {<<"body">>, as, ["tuple"]},
                                                     [{headers, #{compress => false}}]),
        ?assertEqual(Result, Message),
        ?assertMatch({ok, #msg{
                             uuid = <<"76c22bf7-6a92-4a08-8c38-65206e4f51b2">>,
                             to = [<<"to">>],
                             from = <<"from">>,
                             headers = #{compress := false,
                                         binary_body := false},
                             body = {<<"body">>, as, ["tuple"]}}, <<>>},
                     wok_default_message_handler:parse(Message))
    end
   ]}.

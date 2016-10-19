-module(wok_message_path_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("bucs/include/bucassert.hrl").

wok_routes_no_ops_test_() ->
  {setup,
   fun() ->
       doteki:set_env_from_config(
         [{wok, [
           {messages, [
             {controlers, [
               {<<"my_service/my_controler/my_action">>, {dummy_service_handler, my_action}},
               {<<"my_service/my_controler/my_answer">>, {dummy_service_handler, my_answer}},
               {<<"*/my_controler/my_reaction">>, {dummy_service_handler, my_reaction}},
               {<<"my_service/:controler/action/:id">>, {dummy_service_handler, my_parameterized_action}}
             ]}
           ]}
         ]}])
   end,
   fun(_) ->
       ok
   end,
   [
    fun() ->
        ?assertContinueIfMatch(
           #{<<"my_service/my_controler/my_action">> := {dummy_service_handler, my_action},
             <<"my_service/my_controler/my_answer">> := {dummy_service_handler, my_answer},
             <<"*/my_controler/my_reaction">> := {dummy_service_handler, my_reaction},
             <<"my_service/:controler/action/:id">> := {dummy_service_handler, my_parameterized_action}} = X,
           wok_message_path:get_message_path_handlers(doteki:get_env([wok, messages, controlers], [])),
           X,
           fun(Paths) ->
               ?assertEqual([{<<"my_service/my_controler/my_action">>, #{}}],
                            wok_message_path:get_message_handlers(
                              <<"my_service/my_controler/my_action">>,
                              Paths)),
               ?assertEqual([{<<"my_service/my_controler/my_answer">>, #{}}],
                            wok_message_path:get_message_handlers(
                              <<"my_service/my_controler/my_answer">>,
                              Paths)),
               ?assertEqual([{<<"my_service/my_controler/my_answer">>, #{}}],
                            wok_message_path:get_message_handlers(
                              <<"*/my_controler/my_answer">>,
                              Paths)),
               ?assertEqual([{<<"*/my_controler/my_reaction">>, #{}}],
                            wok_message_path:get_message_handlers(
                              <<"*/my_controler/my_reaction">>,
                              Paths)),
               ?assertEqual([{<<"*/my_controler/my_reaction">>, #{}}],
                            wok_message_path:get_message_handlers(
                              <<"bulgroz/my_controler/my_reaction">>,
                              Paths)),
               ?assertEqual([{<<"my_service/my_controler/my_answer">>, #{}}],
                            wok_message_path:get_message_handlers(
                              <<"*/*/my_answer">>,
                              Paths)),
               ?assertEqual([{<<"my_service/my_controler/my_answer">>, #{}}],
                            wok_message_path:get_message_handlers(
                              <<"my_service/*/my_answer">>,
                              Paths)),
               ?assertEqual([{<<"*/my_controler/my_reaction">>, #{}},
                             {<<"my_service/my_controler/my_action">>, #{}},
                             {<<"my_service/my_controler/my_answer">>, #{}}],
                            wok_message_path:get_message_handlers(
                              <<"my_service/my_controler/*">>,
                              Paths)),
               ?assertEqual([{<<"*/my_controler/my_reaction">>, #{}},
                             {<<"my_service/my_controler/my_action">>, #{}},
                             {<<"my_service/my_controler/my_answer">>, #{}}],
                            wok_message_path:get_message_handlers(
                              <<"*/*/*">>,
                              Paths)),
               ?assertEqual([{<<"*/my_controler/my_reaction">>, #{}},
                             {<<"my_service/my_controler/my_action">>, #{}},
                             {<<"my_service/my_controler/my_answer">>, #{}}],
                            wok_message_path:get_message_handlers(
                              <<"*/my_controler/*">>,
                              Paths)),
               ?assertEqual([],
                            wok_message_path:get_message_handlers(
                              <<"*">>,
                              Paths)),
               ?assertEqual([],
                            wok_message_path:get_message_handlers(
                              <<"my_service/my_controler/my_action/hello">>,
                              Paths)),
               ?assertEqual([{<<"my_service/:controler/action/:id">>, #{<<"controler">> => <<"hello_controler">>,
                                                                        <<"id">> => <<"123">>}}],
                            wok_message_path:get_message_handlers(
                              <<"my_service/hello_controler/action/123">>,
                              Paths))
           end)
    end
   ]}.

% @hidden
-module(wok_sup).
-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

-define(CHILD(I, Args, Type, Shutdown), {I, {I, start_link, Args}, permanent, Shutdown, Type, [I]}).

start_link(Static) ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, [Static]).

init([Static]) ->
  Childs = [?CHILD(wok_state, [Static], worker, 5000),
            ?CHILD(wok_middlewares, [], worker, 5000),
            ?CHILD(wok_plugins, [], worker, 5000)] ++
  case doteki:get_env([wok, messages]) of
    undefined ->
      [];
    _ ->
      [
       ?CHILD(wok_consumer_groups, [], worker, 5000)
      ]
  end ++
  case doteki:get_env([wok, producer]) of
    undefined ->
      [];
    _ ->
      [
       ?CHILD(wok_producer_sup, [], supervisor, infinity)
      ]
  end ++ custom_servers() ++ middlewares_servers(),
  {ok, {
     {one_for_one, 5, 10},
     Childs
    }
  }.

custom_servers() ->
  servers(doteki:get_env([wok, start], [])).

middlewares_servers() ->
  lists:foldl(fun({_, Options}, Acc) ->
                  case lists:keyfind(start, 1, Options) of
                    false ->
                      Acc;
                    {start, Servers} ->
                      Acc ++ servers(Servers)
                  end
              end, [], doteki:get_env([wok, middlewares], [])).

servers(Servers) ->
  servers(Servers, []).

servers([], Acc) ->
  lists:reverse(Acc);
servers([{Module, Args, worker}|Rest], Acc) when is_list(Args) ->
  servers(Rest, [?CHILD(Module, Args, worker, 5000)|Acc]);
servers([{Module, Args, supervisor}|Rest], Acc) when is_list(Args) ->
  servers(Rest, [?CHILD(Module, Args, supervisor, infinity)|Acc]);
servers([{Module, Args}|Rest], Acc) when is_list(Args) ->
  servers(Rest, [?CHILD(Module, Args, worker, 5000)|Acc]);
servers([_|Rest], Acc) ->
  servers(Rest, Acc).

-ifdef(TEST).
wok_sup_init_test_() ->
  {setup,
   fun() ->
       meck:new(doteki),
       meck:expect(doteki, get_env, fun
                                      ([wok, messages]) -> defined;
                                      ([wok, producer]) -> defined
                                    end),
       meck:expect(doteki, get_env, fun
                                      ([wok, start], _) ->
                                        [
                                         {worker1, [], worker},
                                         {worker2, [arg1, arg2], worker},
                                         {worker3, []},
                                         {supervisor1, [], supervisor},
                                         {supervisor2, [arg1, arg2], supervisor},
                                         {application1, [], application}
                                        ];
                                      ([wok, middlewares], _) ->
                                        [
                                         {middleware1, [
                                                        {start, [{middleware1_server1, []},
                                                                 {middleware1_server2, [arg1, arg2]}]}
                                                       ]},
                                          {middleware2, []}
                                        ]
                                    end)
   end,
   fun(_) ->
       meck:unload(doteki)
   end,
   [
    fun() ->
        ?assertEqual(
           {ok, {{one_for_one, 5, 10},
                 [
                  ?CHILD(wok_state, [static], worker, 5000),
                  ?CHILD(wok_middlewares, [], worker, 5000),
                  ?CHILD(wok_plugins, [], worker, 5000),
                  ?CHILD(wok_consumer_groups, [], worker, 5000),
                  ?CHILD(wok_producer_sup, [], supervisor, infinity),
                  ?CHILD(worker1, [], worker, 5000),
                  ?CHILD(worker2, [arg1, arg2], worker, 5000),
                  ?CHILD(worker3, [], worker, 5000),
                  ?CHILD(supervisor1, [], supervisor, infinity),
                  ?CHILD(supervisor2, [arg1, arg2], supervisor, infinity),
                  ?CHILD(middleware1_server1, [], worker, 5000),
                  ?CHILD(middleware1_server2, [arg1, arg2], worker, 5000)
                 ]}},
           init([static]))
    end
   ]}.

wok_sup_init_no_messages_test_() ->
  {setup,
   fun() ->
       meck:new(doteki),
       meck:expect(doteki, get_env, fun
                                      ([wok, messages]) -> undefined;
                                      ([wok, producer]) -> defined
                                    end),
       meck:expect(doteki, get_env, fun
                                      ([wok, start], _) ->
                                        [
                                         {worker1, [], worker},
                                         {worker2, [arg1, arg2], worker},
                                         {worker3, []},
                                         {supervisor1, [], supervisor},
                                         {supervisor2, [arg1, arg2], supervisor},
                                         {application1, [], application}
                                        ];
                                      ([wok, middlewares], _) ->
                                        [
                                         {middleware1, [
                                                        {start, [{middleware1_server1, []},
                                                                 {middleware1_server2, [arg1, arg2]}]}
                                                       ]},
                                          {middleware2, []}
                                        ]
                                    end)
   end,
   fun(_) ->
       meck:unload(doteki)
   end,
   [
    fun() ->
        ?assertEqual(
           {ok, {{one_for_one, 5, 10},
                 [
                  ?CHILD(wok_state, [static], worker, 5000),
                  ?CHILD(wok_middlewares, [], worker, 5000),
                  ?CHILD(wok_plugins, [], worker, 5000),
                  ?CHILD(wok_producer_sup, [], supervisor, infinity),
                  ?CHILD(worker1, [], worker, 5000),
                  ?CHILD(worker2, [arg1, arg2], worker, 5000),
                  ?CHILD(worker3, [], worker, 5000),
                  ?CHILD(supervisor1, [], supervisor, infinity),
                  ?CHILD(supervisor2, [arg1, arg2], supervisor, infinity),
                  ?CHILD(middleware1_server1, [], worker, 5000),
                  ?CHILD(middleware1_server2, [arg1, arg2], worker, 5000)
                 ]}},
           init([static]))
    end
   ]}.

wok_sup_init_no_producer_test_() ->
  {setup,
   fun() ->
       meck:new(doteki),
       meck:expect(doteki, get_env, fun
                                      ([wok, messages]) -> defined;
                                      ([wok, producer]) -> undefined
                                    end),
       meck:expect(doteki, get_env, fun
                                      ([wok, start], _) ->
                                        [
                                         {worker1, [], worker},
                                         {worker2, [arg1, arg2], worker},
                                         {worker3, []},
                                         {supervisor1, [], supervisor},
                                         {supervisor2, [arg1, arg2], supervisor},
                                         {application1, [], application}
                                        ];
                                      ([wok, middlewares], _) ->
                                        [
                                         {middleware1, [
                                                        {start, [{middleware1_server1, []},
                                                                 {middleware1_server2, [arg1, arg2]}]}
                                                       ]},
                                          {middleware2, []}
                                        ]
                                    end)
   end,
   fun(_) ->
       meck:unload(doteki)
   end,
   [
    fun() ->
        ?assertEqual(
           {ok, {{one_for_one, 5, 10},
                 [
                  ?CHILD(wok_state, [static], worker, 5000),
                  ?CHILD(wok_middlewares, [], worker, 5000),
                  ?CHILD(wok_plugins, [], worker, 5000),
                  ?CHILD(wok_consumer_groups, [], worker, 5000),
                  ?CHILD(worker1, [], worker, 5000),
                  ?CHILD(worker2, [arg1, arg2], worker, 5000),
                  ?CHILD(worker3, [], worker, 5000),
                  ?CHILD(supervisor1, [], supervisor, infinity),
                  ?CHILD(supervisor2, [arg1, arg2], supervisor, infinity),
                  ?CHILD(middleware1_server1, [], worker, 5000),
                  ?CHILD(middleware1_server2, [arg1, arg2], worker, 5000)
                 ]}},
           init([static]))
    end
   ]}.

wok_sup_init_no_start_test_() ->
  {setup,
   fun() ->
       meck:new(doteki),
       meck:expect(doteki, get_env, fun
                                      ([wok, messages]) -> defined;
                                      ([wok, producer]) -> defined
                                    end),
       meck:expect(doteki, get_env, fun
                                      ([wok, start], _) ->
                                        [];
                                      ([wok, middlewares], _) ->
                                        [
                                         {middleware1, [
                                                        {start, [{middleware1_server1, []},
                                                                 {middleware1_server2, [arg1, arg2]}]}
                                                       ]},
                                          {middleware2, []}
                                        ]
                                    end)
   end,
   fun(_) ->
       meck:unload(doteki)
   end,
   [
    fun() ->
        ?assertEqual(
           {ok, {{one_for_one, 5, 10},
                 [
                  ?CHILD(wok_state, [static], worker, 5000),
                  ?CHILD(wok_middlewares, [], worker, 5000),
                  ?CHILD(wok_plugins, [], worker, 5000),
                  ?CHILD(wok_consumer_groups, [], worker, 5000),
                  ?CHILD(wok_producer_sup, [], supervisor, infinity),
                  ?CHILD(middleware1_server1, [], worker, 5000),
                  ?CHILD(middleware1_server2, [arg1, arg2], worker, 5000)
                 ]}},
           init([static]))
    end
   ]}.

wok_sup_init_no_middlewares_test_() ->
  {setup,
   fun() ->
       meck:new(doteki),
       meck:expect(doteki, get_env, fun
                                      ([wok, messages]) -> defined;
                                      ([wok, producer]) -> defined
                                    end),
       meck:expect(doteki, get_env, fun
                                      ([wok, start], _) ->
                                        [
                                         {worker1, [], worker},
                                         {worker2, [arg1, arg2], worker},
                                         {worker3, []},
                                         {supervisor1, [], supervisor},
                                         {supervisor2, [arg1, arg2], supervisor},
                                         {application1, [], application}
                                        ];
                                      ([wok, middlewares], _) ->
                                        []
                                    end)
   end,
   fun(_) ->
       meck:unload(doteki)
   end,
   [
    fun() ->
        ?assertEqual(
           {ok, {{one_for_one, 5, 10},
                 [
                  ?CHILD(wok_state, [static], worker, 5000),
                  ?CHILD(wok_middlewares, [], worker, 5000),
                  ?CHILD(wok_plugins, [], worker, 5000),
                  ?CHILD(wok_consumer_groups, [], worker, 5000),
                  ?CHILD(wok_producer_sup, [], supervisor, infinity),
                  ?CHILD(worker1, [], worker, 5000),
                  ?CHILD(worker2, [arg1, arg2], worker, 5000),
                  ?CHILD(worker3, [], worker, 5000),
                  ?CHILD(supervisor1, [], supervisor, infinity),
                  ?CHILD(supervisor2, [arg1, arg2], supervisor, infinity)
                 ]}},
           init([static]))
    end
   ]}.
-endif.

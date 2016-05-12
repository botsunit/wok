% @hidden
-module(wok_topics).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include_lib("wok_message_handler/include/wok_message_handler.hrl").
-include("../include/wok.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([consume/5]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
  case doteki:get_env([wok, messages, consumer_group]) of
    undefined ->
      lager:error("Missing consumer group in configuration"),
      {stop, missing_consumer_group};
    ConsumerGroup ->
      LocalQueue = bucs:to_atom(
                     doteki:get_env([wok, messages, local_queue_name],
                                    ?DEFAULT_LOCAL_QUEUE)),
      erlang:send_after(1000, self(), manage),
      {ok, #{consumer_group_prefix => ConsumerGroup,
             local_queue => LocalQueue,
             topics => start_groups(doteki:get_env([wok, messages, topics], []), ConsumerGroup, LocalQueue, [])}}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({consume, Topic, Partition, _Offset, Key, Value}, #{topics := Topics} = State) ->
  case lists:keyfind(Topic, 1, Topics) of
    {Topic, ConsumeMethod, LocalQueues, ServiceNames, _, _, _} ->
      _ = wok_dispatcher:handle(#message_transfert{
                                   key = Key,
                                   message = wok_msg:set_message(wok_msg:new(), Value),
                                   topic = Topic,
                                   partition = Partition,
                                   local_queue = maps:get(Partition, LocalQueues),
                                   service_name = maps:get(Partition, ServiceNames),
                                   consume_method = ConsumeMethod});
    {Topic, ConsumeMethod, LocalQueues, ServiceNames, _} ->
      _ = wok_dispatcher:handle(#message_transfert{
                                   key = Key,
                                   message = wok_msg:set_message(wok_msg:new(), Value),
                                   topic = Topic,
                                   partition = Partition,
                                   local_queue = maps:get(Partition, LocalQueues),
                                   service_name = maps:get(Partition, ServiceNames),
                                   consume_method = ConsumeMethod});
    _ ->
      lager:error("Unregistrered topic ~p", [Topic])
  end,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(manage, #{consumer_group_prefix := ConsumerGroup,
                      local_queue := LocalQueue,
                      topics := Topics} = State) ->
  erlang:send_after(1000, self(), manage),
  {noreply, State#{
              topics => start_groups(Topics, ConsumerGroup, LocalQueue, [])}};
handle_info({'DOWN', MRef, _, _, _}, #{topics := Topics} = State) ->
  _ = erlang:demonitor(MRef),
  {noreply,
   State#{topics => case lists:keyfind(MRef, 7, Topics) of
                      {Name, ConsumeMethod, LocalQueue, ServiceName, Options, _, MRef} ->
                        lists:keyreplace(MRef, 7, Topics, {Name, ConsumeMethod, LocalQueue, ServiceName, Options});
                      false ->
                        Topics
                    end}};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start_groups([], _, _, Acc) ->
  Acc;
start_groups([{Topic, Options}|Rest], CGPrefix, LQPrefix, Acc) ->
  start_groups([{Topic, one_for_all, Options}|Rest], CGPrefix, LQPrefix, Acc);
start_groups([{Topic, ConsumeMethod, Options}|Rest], CGPrefix, LQPrefix, Acc) when ConsumeMethod == one_for_all;
                                                                                   ConsumeMethod == one_for_one ->
  case kafe:topics() of
    #{Topic := Partitions} ->
      LocalQueues = local_queues(ConsumeMethod, LQPrefix, Topic, maps:keys(Partitions)),
      ServiceNames = service_names(LocalQueues),
      start_groups([{Topic, ConsumeMethod, LocalQueues, ServiceNames, Options}|Rest], CGPrefix, LQPrefix, Acc);
    _ ->
      lager:error("Topic ~p does not exist in Kafka !", [Topic]),
      erlang:exit(missing_topic)
  end;
start_groups([{Topic, Other, _}|_], _, _, _) ->
  lager:error("Invalid consumer method ~p for tomic ~p !", [Other, Topic]),
  exit(invalid_consumer_method);
start_groups([{Topic, ConsumeMethod, LocalQueues, ServiceNames, Options}|Rest], CGPrefix, LQPrefix, Acc) ->
  case local_queues_ready(LocalQueues) of
    true ->
      ConsumerGroup = <<CGPrefix/binary, "_", Topic/binary>>,
      case kafe:start_consumer(ConsumerGroup,
                               fun ?MODULE:consume/5,
                               group_options([{topics, [Topic]}|Options])) of
        {ok, PID} ->
          MRef = erlang:monitor(process, PID),
          lager:debug("Start consumer ~p for topic ~p (~p)", [ConsumerGroup, Topic, ConsumeMethod]),
          start_groups(Rest, CGPrefix, LQPrefix,
                       [{Topic, ConsumeMethod, LocalQueues, ServiceNames, Options, PID, MRef}|Acc]);
        {error, Error} ->
          lager:error("Can't start consumer ~p for topic ~p (~p) : ~p", [ConsumerGroup, Topic, ConsumeMethod, Error]),
          start_groups(Rest, CGPrefix, LQPrefix,
                       [{Topic, ConsumeMethod, LocalQueues, ServiceNames, Options}|Acc])
      end;
    false ->
      start_groups(Rest, CGPrefix, LQPrefix, [{Topic, ConsumeMethod, LocalQueues, ServiceNames, Options}|Acc])
  end;
start_groups([{_, _, _, _, _, _, _} = G|Rest], CGPrefix, LQPrefix, Acc) ->
  start_groups(Rest, CGPrefix, LQPrefix, [G|Acc]).

local_queues(ConsumeMethod, Prefix, Topic, Partitions) ->
  lists:foldl(fun(P, Acc) ->
                  maps:put(P, local_queue(ConsumeMethod, Prefix, Topic, P),Acc)
              end, #{}, Partitions).

local_queue(one_for_one, Prefix, Topic, Partition) ->
  bucs:to_atom(<<(bucs:to_binary(Prefix))/binary, "_",
                 (bucs:to_binary(Topic))/binary, "_",
                 (bucs:to_binary(Partition))/binary>>);
local_queue(one_for_all, Prefix, Topic, _) ->
  bucs:to_atom(<<(bucs:to_binary(Prefix))/binary, "_",
                 (bucs:to_binary(Topic))/binary>>).

service_names(Names) ->
  maps:map(fun(_, N) ->
               bucs:to_atom(hexstring(crypto:hash(sha256, bucs:to_binary(N))))
           end, Names).

local_queues_ready(Queues) ->
  maps:fold(fun(_, Q, Acc) ->
                case pipette:ready(Q) of
                  missing_queue ->
                    case pipette:new_queue(Q) of
                      {ok, _} ->
                        Acc and false;
                      {error, Error} ->
                        lager:error("Faild to create local queue ~p: ~p", [Q, Error]),
                        exit(Error)
                    end;
                  Other ->
                    Acc and Other
                end
            end, true, Queues).




group_options(Options) ->
  group_options(Options, #{}).
group_options([], Acc) ->
  Acc;
group_options([{fetch_frequency, Value}|Rest], Acc) ->
  group_options(Rest, Acc#{fetch_interval => Value});
group_options([{max_messages, Value}|Rest], Acc) ->
  group_options(Rest, Acc#{fetch_size => Value});
group_options([{Key, Value}|Rest], Acc) ->
  group_options(Rest, maps:put(Key, Value, Acc)).

consume(Topic, Partition, Offset, Key, Value) ->
  gen_server:cast(?MODULE, {consume, Topic, Partition, Offset, Key, Value}).

hexstring(<<X:128/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~128.16.0b", [X])).

-ifdef(TEST).
start_groups_ok_test() ->
  FakePID = c:pid(0,0,0),
  meck:new(kafe),
  meck:expect(kafe, topics, 0, #{<<"test">> => #{0 => broker0, 1 => broker1, 2 => broker2}}),
  meck:expect(kafe, start_consumer, 3, {ok, FakePID}),
  meck:new(pipette),
  meck:expect(pipette, ready, 1, true),

  ?assertMatch(
     [{<<"test">>,
       one_for_all,
       #{0 := local_queue_test,
         1 := local_queue_test,
         2 := local_queue_test},
       #{0 := 'c84bacbef763e77dced0eb267ae9cdf879ae71abeac8f9d7f81695180d047507',
         1 := 'c84bacbef763e77dced0eb267ae9cdf879ae71abeac8f9d7f81695180d047507',
         2 := 'c84bacbef763e77dced0eb267ae9cdf879ae71abeac8f9d7f81695180d047507'},
       [],
       FakePID,
       _}],
     start_groups([{<<"test">>, []}], <<"CG_PREFIX">>, local_queue, [])),
  ?assertMatch(
     [{<<"test">>,
       one_for_one,
       #{0 := local_queue_test_0,
         1 := local_queue_test_1,
         2 := local_queue_test_2},
       #{0 := 'edaa1f6ed74e45b0e1d36e4f89df3033c47955178e02c72e067f1e7ea5bcc6ab',
         1 := '5cb5ec96970784cd66f190024e947f38a1d18f4c5ac144de63ef7115ba6fbdb1',
         2 := 'e62ec3f5b429459d2d52ca20e6449e2f09d4785a4a013a244a4d4888a9852b03'},
       [],
       FakePID,
       _}],
     start_groups([{<<"test">>, one_for_one, []}], <<"CG_PREFIX">>, local_queue, [])),

  meck:unload(pipette),
  meck:unload(kafe).

start_groups_missing_queue_test() ->
  FakePID = c:pid(0,0,0),
  meck:new(kafe),
  meck:expect(kafe, topics, 0, #{<<"test">> => #{0 => broker0, 1 => broker1, 2 => broker2}}),
  meck:expect(kafe, start_consumer, 3, {ok, FakePID}),
  meck:new(pipette),
  meck:expect(pipette, ready, 1, missing_queue),
  meck:expect(pipette, new_queue, 1, {ok, fake}),

  ?assertMatch(
     [{<<"test">>,
       one_for_all,
       #{0 := local_queue_test,
         1 := local_queue_test,
         2 := local_queue_test},
       #{0 := 'c84bacbef763e77dced0eb267ae9cdf879ae71abeac8f9d7f81695180d047507',
         1 := 'c84bacbef763e77dced0eb267ae9cdf879ae71abeac8f9d7f81695180d047507',
         2 := 'c84bacbef763e77dced0eb267ae9cdf879ae71abeac8f9d7f81695180d047507'},
       []}],
     start_groups([{<<"test">>, []}], <<"CG_PREFIX">>, local_queue, [])),
  ?assertMatch(
     [{<<"test">>,
       one_for_one,
       #{0 := local_queue_test_0,
         1 := local_queue_test_1,
         2 := local_queue_test_2},
       #{0 := 'edaa1f6ed74e45b0e1d36e4f89df3033c47955178e02c72e067f1e7ea5bcc6ab',
         1 := '5cb5ec96970784cd66f190024e947f38a1d18f4c5ac144de63ef7115ba6fbdb1',
         2 := 'e62ec3f5b429459d2d52ca20e6449e2f09d4785a4a013a244a4d4888a9852b03'},
       []}],
     start_groups([{<<"test">>, one_for_one, []}], <<"CG_PREFIX">>, local_queue, [])),

  meck:unload(pipette),
  meck:unload(kafe).

start_groups_false_test() ->
  FakePID = c:pid(0,0,0),
  meck:new(kafe),
  meck:expect(kafe, topics, 0, #{<<"test">> => #{0 => broker0, 1 => broker1, 2 => broker2}}),
  meck:expect(kafe, start_consumer, 3, {ok, FakePID}),
  meck:new(pipette),
  meck:expect(pipette, ready, 1, false),

  ?assertMatch(
     [{<<"test">>,
       one_for_all,
       #{0 := local_queue_test,
         1 := local_queue_test,
         2 := local_queue_test},
       #{0 := 'c84bacbef763e77dced0eb267ae9cdf879ae71abeac8f9d7f81695180d047507',
         1 := 'c84bacbef763e77dced0eb267ae9cdf879ae71abeac8f9d7f81695180d047507',
         2 := 'c84bacbef763e77dced0eb267ae9cdf879ae71abeac8f9d7f81695180d047507'},
      []}],
     start_groups([{<<"test">>, []}], <<"CG_PREFIX">>, local_queue, [])),
  ?assertMatch(
     [{<<"test">>,
       one_for_one,
       #{0 := local_queue_test_0,
         1 := local_queue_test_1,
         2 := local_queue_test_2},
       #{0 := 'edaa1f6ed74e45b0e1d36e4f89df3033c47955178e02c72e067f1e7ea5bcc6ab',
         1 := '5cb5ec96970784cd66f190024e947f38a1d18f4c5ac144de63ef7115ba6fbdb1',
         2 := 'e62ec3f5b429459d2d52ca20e6449e2f09d4785a4a013a244a4d4888a9852b03'},
       []}],
     start_groups([{<<"test">>, one_for_one, []}], <<"CG_PREFIX">>, local_queue, [])),

  meck:unload(pipette),
  meck:unload(kafe).

group_options_test() ->
  ?assertMatch(#{fetch_interval := 1000,
                 fetch_size := 2000,
                 other_option := other_value},
               group_options([{fetch_frequency, 1000},
                              {max_messages, 2000},
                              {other_option, other_value}])).
-endif.

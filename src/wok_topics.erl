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
      erlang:send_after(1000, self(), manage),
      {ok, #{consumer_group_prefix => ConsumerGroup,
             topics => start_groups(doteki:get_env([wok, messages, topics], []), ConsumerGroup, [])}}
  end.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({consume, Topic, Partition, _Offset, Key, Value}, #{topics := Topics} = State) ->
  case lists:keyfind(Topic, 1, Topics) of
    {Topic, ConsumeMethod, LocalQueue, ServiceName, _, _, _} ->
      _ = wok_dispatcher:handle(#message_transfert{
                                   key = Key,
                                   message = wok_msg:set_message(wok_msg:new(), Value),
                                   topic = Topic,
                                   partition = Partition,
                                   local_queue = LocalQueue,
                                   service_name = ServiceName,
                                   consume_method = ConsumeMethod});
    {Topic, ConsumeMethod, LocalQueue, ServiceName, _} ->
      _ = wok_dispatcher:handle(#message_transfert{
                                   key = Key,
                                   message = wok_msg:set_message(wok_msg:new(), Value),
                                   topic = Topic,
                                   partition = Partition,
                                   local_queue = LocalQueue,
                                   service_name = ServiceName,
                                   consume_method = ConsumeMethod});
    _ ->
      lager:error("Unregistrered topic ~p", [Topic])
  end,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(manage, #{consumer_group_prefix := ConsumerGroup,
                      topics := Topics} = State) ->
  erlang:send_after(1000, self(), manage),
  {noreply, State#{
              topics => start_groups(Topics, ConsumerGroup, [])}};
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

start_groups([], _, Acc) ->
  Acc;
start_groups([{Name, Options}|Rest], Prefix, Acc) ->
  start_groups([{Name, one_for_all, Options}|Rest], Prefix, Acc);
start_groups([{Name, one_for_all, Options}|Rest], Prefix, Acc) ->
  case kafe:topics() of
    #{Name := _} ->
      LocalQueue = bucs:to_atom(
                     doteki:get_env([wok, messages, local_queue_name],
                                    ?DEFAULT_LOCAL_QUEUE)),
      start_groups([{Name,
                     one_for_all,
                     bucs:to_atom(LocalQueue),
                     service_name(LocalQueue),
                     Options}|Rest], Prefix, Acc);
    _ ->
      lager:error("Topic ~p does not exist in Kafka", [Name]),
      start_groups(Rest, Prefix, [{Name, one_for_all, Options}|Acc])
  end;
start_groups([{Name, one_for_one, Options}|Rest], Prefix, Acc) ->
  case kafe:topics() of
    #{Name := Topic} ->
      start_groups(
        [{Name,
          {one_for_one, maps:keys(Topic)},
          Options}|Rest], Prefix, Acc);
    _ ->
      lager:error("Topic ~p does not exist in Kafka", [Name]),
      start_groups(Rest, Prefix, [{Name, one_for_one, Options}|Acc])
  end;
start_groups([{Name, {one_for_one, Partitions}, Options}|Rest], Prefix, Acc) when is_list(Partitions) ->
  case kafe:topics() of
    #{Name := Topic} ->
      LocalQueue = bucs:to_atom(
                     doteki:get_env([wok, messages, local_queue_name],
                                    ?DEFAULT_LOCAL_QUEUE)),
      LocalQueueFun = fun(P) ->
                          bucs:to_atom(<<(bucs:to_binary(LocalQueue))/binary, "_",
                                         Name/binary, "_",
                                         (bucs:to_binary(P))/binary>>)
                      end,
      TopicPartitions = maps:keys(Topic),
      start_groups(
        [{Name,
          one_for_one,
          LocalQueueFun(P),
          service_name(LocalQueueFun(P)),
          lists:keystore(partition, 1, Options, {partition, P})}
         || P <- Partitions, lists:member(P, TopicPartitions)] ++ Rest, Prefix, Acc);
    _ ->
      lager:error("Topic ~p does not exist in Kafka", [Name]),
      start_groups(Rest, Prefix, [{Name, one_for_one, Options}|Acc])
  end;
start_groups([{Name, ConsumeMethod, LocalQueue, ServiceName, Options}|Rest], Prefix, Acc) ->
  case pipette:ready(LocalQueue) of
    true ->
      {ConsumerGroup,
       Topics} = case lists:keyfind(partition, 1, Options) of
                   {partition, P} ->
                     {<<Prefix/binary, "_", Name/binary, "_", (bucs:to_binary(P))/binary>>,
                      [{Name, [P]}]};
                   false ->
                     {<<Prefix/binary, "_", Name/binary>>,
                      [Name]}
                 end,
      case kafe:start_consumer(ConsumerGroup, fun ?MODULE:consume/5, group_options([{topics, Topics}|Options])) of
        {ok, PID} ->
          MRef = erlang:monitor(process, PID),
          lager:debug("Start consumer ~p for ~p", [ConsumerGroup, Topics]),
          start_groups(Rest, Prefix, [{Name, ConsumeMethod, LocalQueue, ServiceName, Options, PID, MRef}|Acc]);
        {error, Reason} ->
          lager:error("Can't start group for topic ~p: ~p", [Name, Reason]),
          start_groups(Rest, Prefix, [{Name, ConsumeMethod, LocalQueue, ServiceName, Options}|Acc])
      end;
    missing_queue ->
      case pipette:new_queue(LocalQueue) of
        {ok, _} ->
          lager:info("Local queue ~p created", [LocalQueue]),
          start_groups(Rest, Prefix, [{Name, ConsumeMethod, LocalQueue, ServiceName, Options}|Acc]);
        {error, Reason} ->
          lager:error("Faild to create local queue ~p: ~p", [LocalQueue, Reason]),
          exit(Reason)
      end;
    false ->
      lager:debug("Missing local queue: ~p", [LocalQueue]),
      start_groups(Rest, Prefix, [{Name, ConsumeMethod, LocalQueue, ServiceName, Options}|Acc])
  end;
start_groups([{_, _, _, _, _, _, _} = Topic|Rest], Prefix, Acc) ->
  start_groups(Rest, Prefix, [Topic|Acc]).

group_options(Options) ->
  group_options(Options, #{}).
group_options([], Acc) ->
  Acc;
group_options([{fetch_frequency, Value}|Rest], Acc) ->
  group_options(Rest, Acc#{fetch_interval => Value});
group_options([{max_messages, Value}|Rest], Acc) ->
  group_options(Rest, Acc#{fetch_size => Value});
group_options([{partition, _}|Rest], Acc) ->
  group_options(Rest, Acc);
group_options([{Key, Value}|Rest], Acc) ->
  group_options(Rest, maps:put(Key, Value, Acc)).

consume(Topic, Partition, Offset, Key, Value) ->
  gen_server:cast(?MODULE, {consume, Topic, Partition, Offset, Key, Value}).

service_name(Name) ->
  bucs:to_atom(hexstring(crypto:hash(sha256, bucs:to_binary(Name)))).

hexstring(<<X:128/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~32.16.0b", [X]));
hexstring(<<X:160/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~40.16.0b", [X]));
hexstring(<<X:256/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~64.16.0b", [X]));
hexstring(<<X:512/big-unsigned-integer>>) ->
  lists:flatten(io_lib:format("~128.16.0b", [X])).

-ifdef(TEST).
start_groups_test() ->
  FakePID = c:pid(0,0,0),
  meck:new(kafe),
  meck:expect(kafe, topics, 0, #{<<"test">> => #{0 => broker0, 1 => broker1, 2 => broker2}}),
  meck:expect(kafe, start_consumer, 3, {ok, FakePID}),
  meck:new(pipette),
  meck:expect(pipette, ready, 1, true),

  ?assertMatch([{<<"test">>,
                 one_for_all,
                 local_queue,
                 _,
                 [],
                 FakePID,
                 _}],
               start_groups([{<<"test">>, []}], <<"CG_PREFIX">>, [])),
  ?assertMatch(
     [{<<"test">>,one_for_one,local_queue_test_2,
       _,
       [{partition,2}],
       FakePID,_},
      {<<"test">>,one_for_one,local_queue_test_1,
       _,
       [{partition,1}],
       FakePID,_},
      {<<"test">>,one_for_one,local_queue_test_0,
       _,
       [{partition,0}],
       FakePID,_}],
     start_groups([{<<"test">>, one_for_one, []}], <<"CG_PREFIX">>, [])),
  ?assertMatch(
     [{<<"test">>,one_for_one,local_queue_test_2,
       _,
       [{partition,2}],
       FakePID,_},
      {<<"test">>,one_for_one,local_queue_test_0,
       _,
       [{partition,0}],
       FakePID,_}],
     start_groups([{<<"test">>, {one_for_one, [0, 2]}, []}], <<"CG_PREFIX">>, [])),

  meck:unload(pipette),
  meck:unload(kafe).
-endif.

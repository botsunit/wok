% @hidden
-module(wok_topic).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/wok.hrl").
-define(SERVER, ?MODULE).
-record(topic, {
          fetch_frequency,
          max_bytes,
          max_messages,
          consumer_group,
          name,
          partition,
          local_queue,
          consume
         }).

-export([start_link/2, info/1, fetch/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Name, Options) ->
  LocalQueue = bucs:to_atom(
                 doteki:get_env([wok, messages, local_queue_name],
                                ?DEFAULT_LOCAL_QUEUE)),
  case doteki:get_env([wok, messages, consumer_group]) of
    undefined ->
      lager:error("Missing consumer group in configuration"),
      {error, missing_consumer_group};
    ConsumerGroup ->
      {LocalName,
       LocalQueue1} = case lists:keyfind(partition, 1, Options) of
                        {partition, P} ->
                          {bucs:to_atom(<<Name/binary, "_", (bucs:to_binary(P))/binary>>),
                           bucs:to_atom(<<(bucs:to_binary(LocalQueue))/binary, "_",
                                          Name/binary, "_", (bucs:to_binary(P))/binary>>)};
                        _ -> {bucs:to_atom(Name), LocalQueue}
                      end,
      lager:info("Start topic ~s / server ~s", [Name, LocalName]),
      gen_server:start_link({local, LocalName},
                            ?MODULE,
                            [{consumer_group, ConsumerGroup},
                             {name, Name},
                             {local_queue, LocalQueue1}|Options],
                            [])
  end.

info(TopicLocalName) ->
  gen_server:call(TopicLocalName, info).

fetch(TopicLocalName) ->
  gen_server:call(TopicLocalName, fetch).

%% ------------------------------------------------------------------

init(Args) ->
  Frequency = buclists:keyfind(fetch_frequency, 1, Args, ?DEFAULT_FETCH_FREQUENCY),
  Topic = #topic{
             fetch_frequency = Frequency,
             max_bytes = buclists:keyfind(max_bytes, 1, Args, ?DEFAULT_MESSAGE_MAX_BYTES),
             max_messages = buclists:keyfind(max_messages, 1, Args, ?DEFAULT_MAX_MESSAGES),
             consumer_group = buclists:keyfind(consumer_group, 1, Args),
             name = buclists:keyfind(name, 1, Args),
             partition = buclists:keyfind(partition, 1, Args, all),
             local_queue = buclists:keyfind(local_queue, 1, Args),
             consume = buclists:keyfind(consume, 1, Args)
            },
  % TODO: REMOVE % erlang:send_after(Frequency, self(), fetch),
  {ok, Topic}.

handle_call(info, _From, State) ->
  {reply, State, State};
handle_call(fetch, _From, #topic{fetch_frequency = Frequency,
                                 name = Topic,
                                 consumer_group = ConsumerGroup,
                                 max_bytes = MaxBytes,
                                 max_messages = MaxMessages,
                                 partition = Partition,
                                 local_queue = LocalQueue,
                                 consume = Consume} = State) ->
  lager:debug("Fetch topic ~s #~p", [Topic, Partition]),
  case pipette:ready(LocalQueue) of
    true ->
      Topic1 = if
                 Partition =:= all -> Topic;
                 true -> {Topic, [Partition]}
               end,
      _ = case kafe:offsets(Topic1, ConsumerGroup, MaxMessages) of
            Offsets when is_list(Offsets), Offsets =/= [] ->
              lager:debug("Topic ~p will fetch ~p", [Topic1, Offsets]),
              lists:foreach(
                fun({CurrentPartition, Offset}) ->
                    lager:debug("Fetch message #~p on ~p (partition ~p)", [Offset, Topic, CurrentPartition]),
                    case kafe:fetch(-1, Topic, #{partition => CurrentPartition, offset => Offset, max_bytes => MaxBytes}) of
                      {ok, [#{partitions := CurrentPartitions}]} ->
                        lists:foreach(fun wok_dispatcher:handle/1,
                                      [#message_transfert{
                                          key = Key,
                                          message = wok_msg:set_message(wok_msg:new(), Value),
                                          topic = Topic,
                                          partition = CurrentPartition,
                                          local_queue = LocalQueue,
                                          service_name = service_name(LocalQueue),
                                          consume_method = Consume} ||
                                       #{message := #{key := Key, value := Value}} <- CurrentPartitions,
                                       Value =/= <<>>]);
                      _ ->
                        lager:error("Error fetching message ~p@~p#~p", [Topic, CurrentPartition, Offset])
                    end
                end, Offsets);
            _ ->
              lager:debug("No new message on ~s #~p for ~p", [Topic, Partition, ConsumerGroup])
          end,
      lager:debug("~s#~p next fetch: ~p", [Topic, Partition, Frequency]);
      % TODO: REMOVE % erlang:send_after(Frequency, self(), fetch);
    missing_queue ->
      case pipette:new_queue(LocalQueue) of
        {ok, _} ->
          lager:info("Local queue ~p created", [LocalQueue]);
          % TODO: REMOVE % erlang:send_after(1000, self(), fetch);
        {error, Reason} ->
          lager:error("Faild to create local queue ~p: ~p", [LocalQueue, Reason]),
          exit(Reason)
      end;
    false ->
      lager:debug("Missing local queue: ~p", [LocalQueue])
      % TODO: REMOVE % erlang:send_after(1000, self(), fetch)
  end,
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

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


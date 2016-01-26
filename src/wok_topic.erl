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

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Name, Options) ->
  lager:info("Start topic ~s", [Name]),
  case doteki:get_env([wok, messages, consumer_group]) of
    undefined ->
      lager:error("Missing consumer group in configuration"),
      {error, missing_consumer_group};
    ConsumerGroup ->
      {LocalName,
       LocalQueue} = case lists:keyfind(partition, 1, Options) of
                       {partition, P} ->
                         {bucs:to_atom(<<Name/binary, "_", (bucs:to_binary(P))/binary>>),
                          bucs:to_atom(<<Name/binary, "_", (bucs:to_binary(P))/binary>>)};
                       _ -> {bucs:to_atom(Name),
                             bucs:to_atom(
                               doteki:get_env([wok, messages, local_queue_name],
                                              ?DEFAULT_LOCAL_QUEUE))}
                     end,
      gen_server:start_link({local, LocalName},
                            ?MODULE,
                            [{consumer_group, ConsumerGroup},
                             {name, Name},
                             {local_queue, LocalQueue}|Options],
                            [])
  end.

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
  erlang:send_after(Frequency, self(), fetch),
  {ok, Topic}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(fetch, #topic{fetch_frequency = Frequency,
                          name = Topic,
                          consumer_group = ConsumerGroup,
                          max_bytes = MaxBytes,
                          max_messages = MaxMessages,
                          partition = Partition,
                          local_queue = LocalQueue,
                          consume = Consume} = State) ->
  case pipette:ready(LocalQueue) of
    true ->
      Topic1 = if
                 Partition =:= all -> Topic;
                 true -> {Topic, [Partition]}
               end,
      lager:debug("Fetch topic ~p", [Topic1]),
      _ = case kafe:offsets(Topic1, ConsumerGroup, MaxMessages) of
            Offsets when is_list(Offsets), Offsets =/= [] ->
              lager:debug("Topic ~s will fetch ~p", [Topic, Offsets]),
              lists:foreach(
                fun({CurrentPartition, Offset}) ->
                    lager:debug("Fetch message #~p on ~p (partition ~p)", [Offset, Topic, CurrentPartition]),
                    case kafe:fetch(-1, Topic, #{partition => CurrentPartition, offset => Offset, max_bytes => MaxBytes}) of
                      {ok, [#{partitions := CurrentPartitions}]} ->
                        lists:foreach(fun wok_dispatcher:handle/1,
                                      [#message_transfert{
                                          key = Key,
                                          message = Value,
                                          topic = Topic,
                                          partition = CurrentPartition,
                                          local_queue = LocalQueue,
                                          consume_method = Consume} ||
                                       #{message := #{key := Key, value := Value}} <- CurrentPartitions,
                                       Value =/= <<>>]);
                      _ ->
                        lager:error("Error fetching message ~p@~p#~p", [Topic, CurrentPartition, Offset])
                    end
                end, Offsets);
            _ ->
              lager:debug("No new message on ~p for ~p", [Topic, ConsumerGroup])
          end,
      erlang:send_after(Frequency, self(), fetch);
    false ->
      erlang:send_after(1000, self(), fetch)
  end,
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


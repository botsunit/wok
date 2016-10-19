% @hidden
-module(wok_consumer_groups).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).
-export([assignment_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @hidden
init(_) ->
  case wok_utils:consumer_group() of
    undefined ->
      lager:error("Missing consumer group in configuration"),
      {stop, missing_consumer_group};
    ConsumerGroup ->
      erlang:send_after(1000, self(), manage),
      {ok, #{consumer_group_prefix => ConsumerGroup,
             topics => start_groups(doteki:get_env([wok, messages, topics], []), ConsumerGroup, [])}}
  end.

% @hidden
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(manage, #{consumer_group_prefix := ConsumerGroup,
                      topics := Topics} = State) ->
  erlang:send_after(1000, self(), manage),
  {noreply, State#{topics => start_groups(Topics, ConsumerGroup, [])}};
handle_info({'DOWN', MRef, _, _, _}, #{topics := Topics} = State) ->
  _ = erlang:demonitor(MRef),
  {noreply, State#{topics => case lists:keyfind(MRef, 3, Topics) of
                               {Topic, _, MRef, Options} ->
                                 lists:keyreplace(MRef, 3, Topic, {Topic, Options});
                               false ->
                                 Topics
                             end}};
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start_groups([], _, Acc) ->
  Acc;
start_groups([{Topic, Options}|Rest], CGPrefix, Acc) ->
  ConsumerGroup = <<CGPrefix/binary, "_", Topic/binary>>,
  case kafe:start_consumer(ConsumerGroup,
                           wok_kafe_subscriber,
                           group_options([{topics, [Topic]},
                                          {on_assignment_change, fun ?MODULE:assignment_change/3}
                                          |Options])) of
    {ok, PID} ->
      MRef = erlang:monitor(process, PID),
      lager:debug("Start consumer ~p for topic ~p", [ConsumerGroup, Topic]),
      start_groups(Rest, CGPrefix, [{Topic, PID, MRef, Options}|Acc]);
    {error, Error} ->
      lager:error("Can't start consumer ~p for topic ~p : ~p", [ConsumerGroup, Topic, Error]),
      start_groups(Rest, CGPrefix, [{Topic, Options}|Acc])
  end;
start_groups([{Topic, PID, MRef, Options}|Rest], CGPrefix, Acc) ->
  case erlang:is_process_alive(PID) of
    true ->
      start_groups(Rest, CGPrefix, [{Topic, PID, MRef, Options}|Acc]);
    false ->
      erlang:demonitor(MRef),
      start_groups([{Topic, Options}|Rest], CGPrefix, Acc)
  end.

group_options(Options) ->
  group_options(Options, #{}).
group_options([], Acc) ->
  Acc;
group_options([{fetch_frequency, Value}|Rest], Acc) ->
  group_options(Rest, Acc#{fetch_interval => Value});
group_options([{Key, Value}|Rest], Acc) ->
  group_options(Rest, maps:put(Key, Value, Acc)).

assignment_change(_GroupID, UnAssigned, ReAssigned) ->
  [wok_producer_srv:stop(Topic, Partition) || {Topic, Partition} <- UnAssigned],
  [wok_producer_srv:start(Topic, Partition) || {Topic, Partition} <- ReAssigned],
  ok.


% @hidden
-module(wok_consumer_groups).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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
                                 lists:keyreplace(MRef, 3, Topics, {Topic, Options});
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
  [wok_producer:stop(Topic, Partition)
   || {Topic, Partition} <- UnAssigned],
  [wok_producer:start(Topic, Partition)
   || {Topic, Partition} <- ReAssigned],
  ok.

-ifdef(TEST).
wok_consumer_groups_start_groups_test_() ->
  {setup,
   fun() ->
     meck:new(kafe),
     meck:expect(kafe, start_consumer, 3, {ok, c:pid(0, 0, 0)})
   end,
   fun(_) ->
     meck:unload(kafe)
   end,
   [
     fun() ->
         ?assertMatch(
            [{<<"topic3">>, _PID3, _MRef3, [{test, 3}]},
             {<<"topic2">>, _PID2, _MRef2, [{test, 2}]},
             {<<"topic1">>, _PID1, _MRef1, [{test, 1}]}],
            start_groups([{<<"topic1">>, [{test, 1}]},
                          {<<"topic2">>, [{test, 2}]},
                          {<<"topic3">>, [{test, 3}]}], <<"prefix">>, []))
     end
   ]}.

wok_consumer_groups_start_groups_error_test_() ->
  {setup,
   fun() ->
     meck:new(kafe),
     meck:expect(kafe, start_consumer, 3, {error, test_error})
   end,
   fun(_) ->
     meck:unload(kafe)
   end,
   [
     fun() ->
         ?assertMatch(
            [{<<"topic3">>, [{test, 3}]},
             {<<"topic2">>, [{test, 2}]},
             {<<"topic1">>, [{test, 1}]}],
            start_groups([{<<"topic1">>, [{test, 1}]},
                          {<<"topic2">>, [{test, 2}]},
                          {<<"topic3">>, [{test, 3}]}], <<"prefix">>, []))
     end
   ]}.

wok_consumer_groups_start_groups_half_test_() ->
  {setup,
   fun() ->
     meck:new(kafe),
     meck:expect(kafe, start_consumer, 3, meck:seq([{ok, c:pid(0, 0, 0)},
                                                    {error, test_error},
                                                    {ok, c:pid(0, 0, 0)},
                                                    {error, test_error}]))
   end,
   fun(_) ->
     meck:unload(kafe)
   end,
   [
     fun() ->
         ?assertMatch(
            [{<<"topic4">>, [{test, 4}]},
             {<<"topic3">>, _PID3, _MRef3, [{test, 3}]},
             {<<"topic2">>, [{test, 2}]},
             {<<"topic1">>, _PID1, _MRef1, [{test, 1}]}],
            start_groups([{<<"topic1">>, [{test, 1}]},
                          {<<"topic2">>, [{test, 2}]},
                          {<<"topic3">>, [{test, 3}]},
                          {<<"topic4">>, [{test, 4}]}], <<"prefix">>, []))
     end
   ]}.

wok_consumer_groups_restart_test_() ->
  {setup,
   fun() ->
     meck:new(kafe),
     meck:expect(kafe, start_consumer, 3, {ok, c:pid(0, 0, 0)})
   end,
   fun(_) ->
     meck:unload(kafe)
   end,
   [
     fun() ->
         PID = c:pid(0, 0, 0),
         MRef = erlang:make_ref(),
         ?assertMatch(
            [{<<"topic3">>, PID, _MRef3, [{test, 3}]},
             {<<"topic2">>, PID, _MRef2, [{test, 2}]},
             {<<"topic1">>, PID, MRef, [{test, 1}]}],
            start_groups([{<<"topic1">>, PID, MRef, [{test, 1}]},
                          {<<"topic2">>, c:pid(0, 1000, 0), MRef, [{test, 2}]},
                          {<<"topic3">>, [{test, 3}]}], <<"pwrefix">>, []))
     end
   ]}.

wok_consumer_groups_options_test_() ->
  {setup,
   fun() ->
     ok
   end,
   fun(_) ->
     ok
   end,
   [
     fun() ->
       ?assertMatch(
          #{option1 := 1,
            option2 := 2,
            option3 := 3},
          group_options([{option1, 1},
                         {option2, 2},
                         {option3, 3}]))
     end,
     fun() ->
       ?assertMatch(
          #{option1 := 1,
            fetch_interval := 2,
            option3 := 3},
          group_options([{option1, 1},
                         {fetch_frequency, 2},
                         {option3, 3}]))
     end
   ]}.
-endif.

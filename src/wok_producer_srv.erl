% @hidden
-module(wok_producer_srv).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% API
-export([
         start_link/0
         , start/0
         , start/1
         , start/2
         , stop/0
         , stop/1
         , stop/2
         , pause/0
         , pause/1
         , pause/2
        ]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  todo. % TODO

start(_Topic) ->
  todo. % TODO

start(Topic, Partition) ->
  gen_server:call(?SERVER, {start, Topic, Partition}).

stop() ->
  todo. % TODO

stop(_Topic) ->
  todo. % TODO

stop(Topic, Partition) ->
  gen_server:call(?SERVER, {stop, Topic, Partition}).

pause() ->
  todo. % TODO

pause(_Topic) ->
  todo. % TODO

pause(Topic, Partition) ->
  gen_server:call(?SERVER, {pause, Topic, Partition}).

% @hidden
init([]) ->
  {ok, []}.

% @hidden
handle_call({start, Topic, Partition}, _From, State) ->
  {Reply, State1} = start(Topic, Partition, State),
  {reply, Reply, State1};
handle_call({stop, Topic, Partition}, _From, State) ->
  case lists:keyfind({Topic, Partition}, 1, State) of
    {{Topic, Partition}, PID, MRef} ->
      _ = erlang:demonitor(MRef),
      case wok_producer_async_sup:stop_child(PID) of
        ok ->
          {reply, ok, lists:keydelete({Topic, Partition}, 1, State)};
        {error, Reason} ->
          lager:error("Faild to stop producer for ~p#~p : ~p", [Topic, Partition, Reason]),
          {reply, {error, Reason}, State}
      end;
    false ->
      {reply, ok, State}
  end;
handle_call({pause, Topic, Partition}, _From, State) ->
  case lists:keyfind({Topic, Partition}, 1, State) of
    {{Topic, Partition}, PID, _} ->
      wok_producer_async_srv:call(PID, pause);
    false ->
      ok
  end,
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  {reply, ignore, State}.

% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info({'DOWN', MRef, _, _, _}, State) ->
  _ = erlang:demonitor(MRef),
  case lists:keyfind(MRef, 3, State) of
    {{Topic, Partition}, _, MRef} ->
      lager:info("Producer for ~p#~p DOWN... Restarting", [Topic, Partition]),
      State1 = lists:keydelete(MRef, 3, State),
      {_, State2} = start(Topic, Partition, State1),
      {noreply, State2};
    false ->
      {noreply, State}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

start(Topic, Partition, State) ->
  case lists:keyfind({Topic, Partition}, 1, State) of
    {{Topic, Partition}, PID, _} ->
      {gen_server:call(PID, start), State};
    false ->
      case wok_producer_async_sup:start_child([Topic, Partition]) of
        {ok, PID} ->
          MRef = erlang:monitor(process, PID),
          {ok, [{{Topic, Partition}, PID, MRef}|State]};
        {error, Reason} ->
          lager:error("Faild to start ~p#~p: ~p", [Topic, Partition, Reason]),
          {{error, Reason}, State}
      end
  end.


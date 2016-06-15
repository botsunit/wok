% @hidden
-module(wok_producer_async_srv).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/wok.hrl").

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          topic,
          partition,
          handler,
          timer,
          frequency,
          size
         }).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

% @hidden
init([Topic, Partition]) ->
  lager:info("Producer for ~p#~p started", [Topic, Partition]),
  Handler = doteki:get_env([wok, producer, handler]),
  Frequency = doteki:get_env([wok, producer, frequency], ?DEFAULT_PRODUCER_FREQUENCY),
  Size = doteki:get_env([wok, producer, number_of_messages], ?DEFAULT_PRODUCER_SIZE),
  {ok, #state{
          topic = Topic,
          partition = Partition,
          handler = Handler,
          timer = erlang:send_after(Frequency, self(), produce),
          frequency = Frequency,
          size = Size
         }}.

% @hidden
handle_call(pause, _From, #state{timer = undefined} = State) ->
  {reply, ok, State};
handle_call(pause, _From, #state{timer = Timer} = State) ->
  _ = erlang:cancel_timer(Timer),
  {reply, ok, State#state{timer = undefined}};
handle_call(start, _From, #state{timer = undefined,
                                 frequency = Frequency} = State) ->
  {reply, ok, State#state{timer = erlang:send_after(Frequency, self(), produce)}};
handle_call(start, _From, State) ->
  {reply, ok, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(produce, #state{frequency = Frequency,
                            handler = _Handler} = State) ->
  {noreply, State#state{timer = erlang:send_after(Frequency, self(), produce)}};
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


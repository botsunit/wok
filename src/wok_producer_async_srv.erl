% @hidden
-module(wok_producer_async_srv).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/wok.hrl").
-include_lib("../include/wok_message_handler.hrl").

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
handle_info(produce, #state{topic = Topic,
                            partition = Partition,
                            size = Size,
                            frequency = Frequency,
                            handler = Handler} = State) ->
  Messages = erlang:apply(Handler, messages, [Topic, Partition, Size]),
  case produce(Messages, Topic, Partition, Handler) of
    true ->
      {noreply, State#state{timer = erlang:send_after(Frequency, self(), produce)}};
    false ->
      {noreply, State#state{timer = undefined}}
  end;
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

produce([], _, _, _) ->
  true;
produce([{MessageID, Topic, Partition, Message}|Rest], Topic, Partition, Handler) ->
  produce([{MessageID, Topic, Partition, Message, false}|Rest], Topic, Partition, Handler);
produce([{MessageID, Topic, Partition, Message, Retry} = M|Rest], Topic, Partition, Handler) ->
  lager:info("Produce message #~p on topic ~s partition ~p", [MessageID, Topic, Partition]),
  Response = binary_to_term(base64:decode(Message)),
  Response1 = Response#wok_message{reply = true},
  Result =  case wok_middlewares:outgoing_message(Response1) of
              {ok, Response2} ->
                case wok_message:get_response(Response2) of
                  {reply, _, From, To, Body} ->
                    case wok_message:provide({Topic, Partition}, From, To, Body,
                                             [{headers, #{message_id => MessageID}}]) of
                      {ok, [#{partitions := [#{error_code := none}]}]} ->
                        erlang:apply(Handler, response, [MessageID, ok, Retry]);
                      {ok, [#{partitions := [#{error_code := KafeError}]}]} ->
                        erlang:apply(Handler, response, [MessageID, {error, KafeError}, Retry]);
                      E ->
                        erlang:apply(Handler, response, [MessageID, E, Retry])
                    end;
                  _ ->
                    erlang:apply(Handler, response, [MessageID, {error, invalid_message}, Retry])
                end;
              {stop, Middleware, Reason} = Stop ->
                lager:debug("Middleware ~p stop message ~p reason: ~p", [Middleware, Response1, Reason]),
                erlang:apply(Handler, response, [MessageID, Stop, Reason])
            end,
  provide_response(Result, M, Rest, Topic, Partition, Handler);
produce([{MessageID, _, _, _, Retry} = M|Rest], Topic, Partition, Handler) ->
  provide_response(erlang:apply(Handler, response, [MessageID, {error, wrong_topic_partition}, Retry]),
                   M, Rest, Topic, Partition, Handler).

provide_response(Response, {MessageID, Topic, Partition, Message, _}, Rest, Topic, Partition, Handler) ->
  case Response of
    retry ->
      produce([{MessageID, Topic, Partition, Message, true}|Rest], Topic, Partition, Handler);
    next ->
      produce(Rest, Topic, Partition, Handler);
    stop ->
      lager:debug("Stop producer for ~p#~p", [Topic, Partition]),
      true;
    exit ->
      lager:debug("Exit producer for ~p#~p", [Topic, Partition]),
      false
  end.

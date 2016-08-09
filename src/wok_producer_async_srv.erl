% @hidden
-module(wok_producer_async_srv).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/wok.hrl").
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

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
  KafeNoError = kafe_error:code(0),
  lager:info("Produce message #~p on ~p#~p", [MessageID, Topic, Partition]),
  Response = case binary_to_term(base64:decode(Message)) of
              MessageTransfert = #message_transfert{message = Message0} ->
                case wok_middlewares:outgoing_message(Message0) of
                  {ok, Message1} ->
                    MessageTransfert1 = MessageTransfert#message_transfert{message = Message1},
                    case wok_msg:get_response(MessageTransfert1) of
                      {Topic, From, To, Body} ->
                        case wok_message:provide({Topic, Partition}, From, To, Body,
                                                 [{headers, #{message_id => MessageID}}]) of
                          {ok, [#{partitions := [#{error_code := KafeNoError}]}]} ->
                            erlang:apply(Handler, response, [MessageID, ok, Retry]);
                          {ok, [#{partitions := [#{error_code := KafeError}]}]} ->
                            erlang:apply(Handler, response, [MessageID, {error, KafeError}, Retry]);
                          E ->
                            erlang:apply(Handler, response, [MessageID, E, Retry])
                        end;
                      Other ->
                        erlang:apply(Handler, response, [MessageID, {error, Other}, Retry])
                    end;
                  {stop, Middleware, Reason} = Stop ->
                    lager:debug("Middleware ~p stop message ~p reason: ~p", [Middleware, Message0, Reason]),
                    erlang:apply(Handler, response, [MessageID, Stop, Reason])
                end;
              Msg when is_record(Msg, wok_msg) ->
                case wok_msg:get_response(Msg) of
                  {Topic, From, To, Body} ->
                    case wok_message:provide({Topic, Partition}, From, To, Body,
                                             [{headers, #{message_id => MessageID}}]) of
                      {ok, [#{partitions := [#{error_code := KafeNoError}]}]} ->
                        erlang:apply(Handler, response, [MessageID, ok, Retry]);
                      {ok, [#{partitions := [#{error_code := KafeError}]}]} ->
                        erlang:apply(Handler, response, [MessageID, {error, KafeError}, Retry]);
                      E ->
                        erlang:apply(Handler, response, [MessageID, E, Retry])
                    end;
                  Other ->
                    erlang:apply(Handler, response, [MessageID, {error, Other}, Retry])
                end
            end,
  provide_response(Response, M, Rest, Topic, Partition, Handler);
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

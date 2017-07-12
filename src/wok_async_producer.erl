% @hidden
-module(wok_async_producer).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/wok.hrl").

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
         , send/0
        ]).

-record(state, {
          topics = #{
            started => [],
            paused => []
           },
          handler,
          timer,
          frequency,
          size
         }).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(GEN_SERVER_CALL(Server, Message),
        case erlang:whereis(Server) of
          undefined ->
            {error, server_not_available};
          _ ->
            gen_server:call(Server, Message)
        end).
-define(GEN_SERVER_CAST(Server, Message),
        case erlang:whereis(Server) of
          undefined ->
            {error, server_not_available};
          _ ->
            gen_server:cast(Server, Message)
        end).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

start() ->
  todo.% TODO

start(_Topic) ->
  todo. % TODO

start(Topic, Partition) ->
  ?GEN_SERVER_CALL(?SERVER, {start, Topic, Partition}).

stop() ->
  todo. % TODO

stop(_Topic) ->
  todo. % TODO

stop(Topic, Partition) ->
  ?GEN_SERVER_CALL(?SERVER, {stop, Topic, Partition}).

pause() ->
  todo. % TODO

pause(_Topic) ->
  todo. % TODO

pause(Topic, Partition) ->
  ?GEN_SERVER_CALL(?SERVER, {pause, Topic, Partition}).

send() ->
  ?GEN_SERVER_CAST(?SERVER, send).

% @hidden
init([]) ->
  Handler = doteki:get_env([wok, producer, handler]),
  Frequency = doteki:get_env([wok, producer, frequency], ?DEFAULT_PRODUCER_FREQUENCY),
  Size = doteki:get_env([wok, producer, number_of_messages], ?DEFAULT_PRODUCER_SIZE),
  Topics = wok_async_producer_state:get(),
  {ok, #state{
          topics = Topics,
          handler = Handler,
          timer = erlang:send_after(Frequency, self(), produce),
          frequency = Frequency,
          size = Size
         }}.


% @hidden
handle_call({start, Topic, Partition}, _From, #state{topics = Topics} = State) ->
  Topics0 = topics_add(Topics, started, Topic, Partition),
  Topics1 = topics_remove(Topics0, paused, Topic, Partition),
  wok_async_producer_state:put(Topics1),
  {reply, ok, State#state{topics = Topics1}};
handle_call({stop, Topic, Partition}, _From, #state{topics = Topics} = State) ->
  Topics0 = topics_remove(Topics, paused, Topic, Partition),
  Topics1 = topics_remove(Topics0, started, Topic, Partition),
  wok_async_producer_state:put(Topics1),
  {reply, ok, State#state{topics = Topics1}};
handle_call({pause, Topic, Partition}, _From, #state{topics = Topics} = State) ->
  Topics0 = topics_add(Topics, paused, Topic, Partition),
  Topics1 = topics_remove(Topics0, started, Topic, Partition),
  wok_async_producer_state:put(Topics1),
  {reply, ok, State#state{topics = Topics1}};
handle_call(_Request, _From, State) ->
  {reply, ignore, State}.

% @hidden
handle_cast(send, #state{timer = Timer} = State) ->
  erlang:cancel_timer(Timer),
  handle_info(produce, State#state{timer = undefined});
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(produce, #state{frequency = Frequency,
                            topics = #{started := Topics},
                            handler = Handler,
                            size = Size} = State) ->
  Messages = erlang:apply(Handler, messages, [Topics, Size]),
  case produce(Messages, Handler) of
    ok ->
      {noreply, State#state{timer = erlang:send_after(Frequency, self(), produce)}};
    stop ->
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

topics_add(Topics, Type, Topic, Partition) ->
  List0 = maps:get(Type, Topics, []),
  List1 = case lists:keyfind(Topic, 1, List0) of
            {Topic, Partitions} ->
              case lists:member(Partition, Partitions) of
                true ->
                  List0;
                false ->
                  lists:keyreplace(Topic, 1, List0, {Topic, [Partition|Partitions]})
              end;
            false ->
              [{Topic, [Partition]}|List0]
          end,
  maps:put(Type, List1, Topics).

topics_remove(Topics, Type, Topic, Partition) ->
  List0 = maps:get(Type, Topics, []),
  List1 = case lists:keyfind(Topic, 1, List0) of
            {Topic, Partitions} ->
              case lists:delete(Partition, Partitions) of
                [] ->
                  lists:keydelete(Topic, 1, List0);
                P ->
                  lists:keyreplace(Topic, 1, List0, {Topic, P})
              end;
            false ->
              List0
          end,
  maps:put(Type, List1, Topics).

produce([], _) ->
  ok;
produce(Messages, Handler) ->
  case handle(Messages) of
    {ok, MessagesGroups} ->
      Results = produce_groups(MessagesGroups),
      erlang:apply(Handler, response, [buclists:keyfind(ok, 1, Results, []),
                                       buclists:keyfind(error, 1, Results, [])]);
    {error, Error} ->
      erlang:apply(Handler, response, [[], [Error]])
  end.

produce_groups(MessagesGroups) ->
  produce_groups(MessagesGroups, []).

produce_groups([], Acc) ->
  Acc;
produce_groups([{Messages, IDs}|Rest], Acc) ->
  produce_groups(
    Rest,
    case kafe:produce(Messages) of
      {ok, #{topics := Results}} ->
        result(IDs, Acc, has_error(Results));
      {ok, Results} ->
        result(IDs, Acc, has_error(Results));
      _ ->
        result(IDs, Acc, error)
    end).

result(IDs, Acc, Result) ->
  Current = buclists:keyfind(Result, 1, Acc, []),
  buclists:keyupdate(Result, 1, Acc, {Result, Current ++ IDs}).

has_error([]) ->
  ok;
has_error([#{partitions := Partitions}|Rest]) ->
  has_error(Partitions, Rest).
has_error([], Rest) ->
  has_error(Rest);
has_error([#{error_code := Error}|_], _) when Error =/= none ->
  error;
has_error([_|Rest], Other) ->
  has_error(Rest, Other).

handle(Messages) ->
  handle(Messages, #{}).
handle([], Acc) ->
  {ok, maps:values(Acc)};
handle([{MessageID, Topic, Partition, Message}|Rest], Acc) ->
  Response = binary_to_term(base64:decode(Message)),
  case wok_message:get_response(Response) of
    {reply, _, From, To, Body} ->
      Message0 = wok_producer:handle(From, To, Body, [{headers, #{message_id => MessageID}}]),
      handle(Rest, dispatch(MessageID, Message0, Topic, Partition, Acc));
    _ ->
      {error, {invalid_message, MessageID}}
  end.

dispatch(MessageID, Message, Topic, Partition, Acc) ->
  Broker = kafe_brokers:broker_id_by_topic_and_partition(Topic, Partition),
  {Topics, IDs} = maps:get(Broker, Acc, {[], []}),
  Messages = buclists:keyfind(Topic, 1, Topics, []),
  maps:put(
    Broker,
    {buclists:keyupdate(Topic, 1, Topics, {Topic, Messages ++ [{Message, Partition}]}),
     [MessageID|IDs]},
    Acc).


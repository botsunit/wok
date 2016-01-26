% @hidden
-module(wok_dispatcher).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/wok.hrl").
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-define(SERVER, ?MODULE).

-export([start_link/0, handle/1]).
-export([finish/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

handle(MessageTransfert) ->
  gen_server:cast(?SERVER, {handle, MessageTransfert}).

finish(Child, MessageTransfert) ->
  gen_server:cast(?SERVER, {terminate, Child, MessageTransfert}).

%% ------------------------------------------------------------------

init(_Args) ->
  erlang:send_after(1000, self(), fetch),
  {ok, get_service_handlers(#{})}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({handle, #message_transfert{message = Message} = MessageTransfert}, State) ->
  try
    case erlang:apply(doteki:get_env([wok, messages, handler],
                                     ?DEFAULT_MESSAGE_HANDLER),
                      parse, [Message]) of
      {ok, #message{to = To} = ParsedMessage, _} ->
        _ = consume(MessageTransfert#message_transfert{message = ParsedMessage},
                    get_services(To, State),
                    State);
      {error, Reason} ->
        lager:error("Error parsing message: ~p", [Reason]);
      _ ->
        lager:error("Wrong message parser return. See wok_message_handler for more informations")
    end
  catch
    Class:Reason1 ->
      lager:error("Parser faild: ~p:~p~n~p", [Class, Reason1, erlang:get_stacktrace()])
  end,
  {noreply, State};
handle_cast({terminate, Child, #message_transfert{result = Result,
                                                  local_queue = LocalQueue}}, State) ->
  case Result of
    noreply ->
      ok;
    {reply, Topic, Message} ->
      case wok_middlewares:outgoing_message(Message) of
        {ok, Message1} ->
          case wok:provide(Topic, Message1) of
            {ok, _} ->
              lager:debug("Message provided");
            E ->
              lager:error("Faild to provide message : ~p", [E]) % TODO: Queue message and retry later
          end;
        {stop, Middleware, Reason} ->
          lager:debug("Middleware ~p stop message ~p reason: ~p", [Middleware, Message, Reason])
      end;
    _ ->
      lager:error("Invalid response : ~p", [Result]),
      ignore
  end,
  _ = case wok_services_sup:terminate_child(Child) of
        ok ->
          case doteki:get_env([wok, messages, local_consumer_group],
                              doteki:get_env([wok, messages, consumer_group], undefined)) of
            undefined ->
              lager:error("Missing consumer group in configuration"),
              exit(config_error);
            LocalConsumerGroup ->
              case pipette:out(LocalQueue, #{consumer => LocalConsumerGroup}) of
                {ok, Data} ->
                  force_consume(Data);
                {error, no_data} ->
                  ok;
                {error, E2} ->
                  lager:error("Pipette out error: ~p", [E2]),
                  ok
              end
          end;
        E1 ->
          lager:error("Faild to stop service #~p : ~p", [Child, E1])
      end,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(fetch, State) ->
  case doteki:get_env([wok, messages, local_consumer_group],
                      doteki:get_env([wok, messages, consumer_group], undefined)) of
    undefined ->
      lager:error("Missing consumer group in configuration"),
      exit(config_error);
    LocalConsumerGroup ->
      LocalQueue = bucs:to_atom(
                     doteki:get_env([wok, messages, local_queue_name],
                                    ?DEFAULT_LOCAL_QUEUE)),
      case pipette:ready(LocalQueue) of
        true ->
          [case pipette:out(LocalQueue, #{consumer => LocalConsumerGroup}) of
             {ok, Data} ->
               force_consume(Data);
             {error, no_data} ->
               ok;
             {error, E} ->
               lager:error("Pipette out error : ~p", [E]),
               ok
           end || _ <- lists:seq(1, doteki:get_env([wok, messages, max_services_fork], ?DEFAULT_MAX_SERVICES_FORK))];
        false ->
          erlang:send_after(1000, self(), fetch)
      end
  end,
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_service_handlers(State) ->
  State#{services => lists:foldl(fun({ServiceName, Handler}, Services) ->
                                     maps:put(ServiceName, Handler, Services)
                                 end, #{}, doteki:get_env([wok, messages, services],
                                                          doteki:get_env([wok, messages, controlers], [])))}.

get_services(To, State) when is_binary(To) ->
  get_services([To],State);
get_services(Tos, #{services := Services}) when is_list(Tos) ->
  lists:foldl(fun(To, Acc) ->
                  lists:umerge(lists:sort(Acc),
                               lists:sort(
                                 get_services(
                                   binary:split(To, <<"/">>, [global]),
                                   maps:keys(Services), [])))
              end, [], Tos).

get_services(_, [], Result) ->
  Result;
get_services(To, [Service|Services], Result) ->
  get_services(To, Services, service_match(To, binary:split(bucs:to_binary(Service), <<"/">>, [global]), Service, Result)).

service_match(_, [], ServiceName, Result) ->
  [ServiceName|Result];
service_match([], X, _, Result) when X =/= [] ->
  Result;
service_match([X|_], [Y|_], _, Result) when X =/= Y,
                                            X =/= <<"*">>,
                                            Y =/= <<"*">> ->
  Result;
service_match([X|To], [Y|Service], ServiceName, Result) when X == Y;
                                                             X == <<"*">>;
                                                             Y == <<"*">> ->
  service_match(To, Service, ServiceName, Result).

consume(_, [], _) ->
  ok;
consume(#message_transfert{message = ParsedMessage, local_queue = LocalQueue} = MessageTransfert,
        Services, #{services := ServicesActions}) ->
  case wok_middlewares:incoming_message(ParsedMessage) of
    {ok, ParsedMessage1} ->
      case doteki:get_env([wok, messages, local_consumer_group],
                          doteki:get_env([wok, messages, consumer_group], undefined)) of
        undefined ->
          lager:info("Missing consumer group in configuration"),
          exit(config_error);
        LocalConsumerGroup ->
          lists:foreach(fun(Service) ->
                            LocalConsumerGroupOffset = pipette:offset(LocalQueue, #{consumer => LocalConsumerGroup}),
                            MessageTransfert1 = MessageTransfert#message_transfert{
                                                  message = ParsedMessage1,
                                                  service = Service,
                                                  action = maps:get(Service, ServicesActions)},
                            case pipette:queue(LocalQueue) of
                              {LocalQueue, LocalConsumerGroupOffset, _, _, _} ->
                                case wok_services_sup:start_child(MessageTransfert1) of
                                  {ok, Child} ->
                                    gen_server:cast(Child, serve);
                                  {ok, Child, _} ->
                                    gen_server:cast(Child, serve);
                                  {queue, Data} ->
                                    queue(Data);
                                  {error, Reason} ->
                                    lager:error("Faild to start service : ~p", [Reason]),
                                    error
                                end;
                              {LocalQueue, CurrentConsumerGroupOffset, _, _, _} when CurrentConsumerGroupOffset > LocalConsumerGroupOffset ->
                                queue(MessageTransfert1);
                              Other ->
                                lager:error("Queue ~p error: ~p", [LocalQueue, Other])
                            end
                        end, Services)
      end;
    {stop, Middleware, Reason} ->
      lager:debug("Middleware ~p stop message ~p reason: ~p", [Middleware, ParsedMessage, Reason])
  end.

force_consume(#message_transfert{message = ParsedMessage,
                                 service = Service,
                                 action = Action} = MessageTransfert) ->
  lager:debug("Force consume message ~p:~p(~p)", [Service, Action, ParsedMessage]),
  case wok_services_sup:start_child(MessageTransfert) of
    {ok, Child} ->
      gen_server:cast(Child, serve);
    {ok, Child, _} ->
      gen_server:cast(Child, serve);
    {queue, Data} ->
      force_consume(Data);
    {error, Reason} ->
      lager:error("Faild to start service : ~p", [Reason]),
      error
  end.

queue(#message_transfert{local_queue = LocalQueue, message = ParsedMessage} = Data) ->
  case pipette:in(LocalQueue, Data) of
    {ok, LocalOffset} ->
      lager:debug("Message ~p queued with local offset ~p", [ParsedMessage, LocalOffset]),
      ok;
    {error, Reason} ->
      lager:error("Faild to queue message ~p to ~p: ", [ParsedMessage, LocalQueue, Reason]),
      error
  end.


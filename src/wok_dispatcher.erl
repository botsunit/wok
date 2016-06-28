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
  {ok, #{services => wok_message_path:get_message_path_handlers(
                       doteki:get_env([wok, messages, services],
                                      doteki:get_env([wok, messages, controllers],
                                                     doteki:get_env([wok, messages, controlers], []))))}}. % BW compatibility :/

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({handle, #message_transfert{commit_id = CommitID} = MessageTransfert}, #{services := Services} = State) ->
  try
    Handler = case doteki:get_env([wok, messages, handler], ?DEFAULT_MESSAGE_HANDLER) of
                {Module, _} -> Module;
                Module -> Module
              end,
    case erlang:apply(Handler, parse, [wok_msg:get_message(MessageTransfert)]) of
      {ok, ParsedMessage, _} ->
        _ = consume(wok_msg:set_message(MessageTransfert, ParsedMessage),
                    wok_message_path:get_message_handlers(
                      wok_msg:get_to(ParsedMessage),
                      Services),
                    State);
      {error, Reason} ->
        lager:error("Error parsing message: ~p", [Reason]),
        _ = commit(CommitID);
      _ ->
        lager:error("Wrong message parser return. See wok_message_handler for more informations"),
        _ = commit(CommitID)
    end
  catch
    Class:Reason1 ->
      lager:error("Parser faild: ~p:~p~n~p", [Class, Reason1, erlang:get_stacktrace()])
  end,
  {noreply, State};
handle_cast({terminate, Child, #message_transfert{message = Message,
                                                  local_queue = LocalQueue,
                                                  commit_id = CommitID} = MessageTransfert}, State) ->
  case wok_msg:get_response(MessageTransfert) of
    noreply ->
      ok;
    {_, _, _, _} ->
      case wok_middlewares:outgoing_message(Message) of
        {ok, Message1} ->
          MessageTransfert1 = MessageTransfert#message_transfert{message = Message1},
          case wok_msg:get_response(MessageTransfert1) of
            {Topic, From, To, Body} ->
              case wok_message:provide(Topic, From, To, Body) of
                {ok, _} ->
                  lager:debug("Message provided");
                E ->
                  lager:error("Faild to provide message : ~p", [E]),
                  _ = pipette:clean_all(),
                  init:stop()
              end;
            noreply ->
              ok
          end;
        {stop, Middleware, Reason} ->
          lager:debug("Middleware ~p stop message ~p reason: ~p", [Middleware, Message, Reason])
      end;
    R ->
      lager:error("Message controler faild : ~p", [R]),
      _ = pipette:clean_all(),
      init:stop()
  end,
  _ = commit(CommitID),
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
          erlang:send_after(1000, self(), fetch);
        missing_queue ->
          case pipette:new_queue(LocalQueue) of
            {ok, _} ->
              false;
            {error, Error} ->
              lager:error("Faild to create local queue ~p: ~p", [LocalQueue, Error]),
              init:stop()
          end
      end
  end,
  {noreply, State};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

consume(#message_transfert{commit_id = CommitID}, [], _) ->
  lager:debug("No service for message : commit ~p", [CommitID]),
  case kafe_consumer:commit(CommitID, #{retry => 3}) of
    {error, Reason1} ->
      lager:debug("Commit faild : ~p", [Reason1]),
      _ = pipette:clean_all(),
      init:stop();
    _ ->
      ok
  end;
consume(#message_transfert{message = ParsedMessage,
                           local_queue = LocalQueue} = MessageTransfert,
        Services, #{services := ServicesActions}) ->
  ParsedMessage1 = wok_msg:set_local_state(ParsedMessage, undefined),
  ParsedMessage2 = wok_msg:set_global_state(ParsedMessage1, wok_state:state()),
  case wok_middlewares:incoming_message(ParsedMessage2) of
    {ok, ParsedMessage3} ->
      case doteki:get_env([wok, messages, local_consumer_group],
                          doteki:get_env([wok, messages, consumer_group], undefined)) of
        undefined ->
          lager:info("Missing consumer group in configuration"),
          exit(config_error);
        LocalConsumerGroup ->
          lists:foreach(fun(Service) ->
                            LocalConsumerGroupOffset = pipette:offset(LocalQueue,
                                                                      #{consumer => LocalConsumerGroup}),
                            MessageTransfert1 = MessageTransfert#message_transfert{
                                                  message = ParsedMessage3,
                                                  service = Service,
                                                  action = maps:get(Service, ServicesActions)},
                            MessageTransfert2 = wok_msg:set_local_state(MessageTransfert1, undefined),
                            case pipette:queue(LocalQueue) of
                              {LocalQueue, LocalConsumerGroupOffset, _, _, _} ->
                                case wok_services_sup:start_child(MessageTransfert2) of
                                  {ok, Child} ->
                                    gen_server:cast(Child, serve);
                                  {queue, Data} ->
                                    queue(Data);
                                  {error, Reason, Data} ->
                                    lager:error("Faild to start service : ~p", [Reason]),
                                    queue(Data)
                                end;
                              {LocalQueue, CurrentConsumerGroupOffset, _, _, _} when
                                  CurrentConsumerGroupOffset > LocalConsumerGroupOffset ->
                                queue(MessageTransfert2);
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
  MessageTransfert1 = wok_msg:set_local_state(MessageTransfert, undefined),
  MessageTransfert2 = wok_msg:set_global_state(MessageTransfert1, wok_state:state()),
  case wok_services_sup:start_child(MessageTransfert2) of
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

queue(#message_transfert{local_queue = LocalQueue, message = ParsedMessage} = MessageTransfert) ->
  MessageTransfert1 = wok_msg:set_local_state(MessageTransfert, undefined),
  MessageTransfert2 = wok_msg:set_global_state(MessageTransfert1, undefined),
  case pipette:in(LocalQueue, MessageTransfert2) of
    {ok, LocalOffset} ->
      lager:debug("Message ~p queued with local offset ~p", [ParsedMessage, LocalOffset]),
      ok;
    {error, Reason} ->
      lager:error("Faild to queue message ~p to ~p: ", [ParsedMessage, LocalQueue, Reason]),
      error
  end.

commit(CommitID) ->
  case kafe_consumer:commit(CommitID, #{retry => 3}) of
    {error, Reason1} ->
      lager:debug("Commit faild : ~p", [Reason1]),
      _ = pipette:clean_all(),
      init:stop();
    _ ->
      ok
  end.


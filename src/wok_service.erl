% @hidden
-module(wok_service).
-behaviour(gen_server).
-include("../include/wok.hrl").
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Message) ->
  gen_server:start_link(?MODULE, Message, []).

init({<<>>, Message}) ->
  lager:info("Start service with message ~p", [Message]),
  {ok, get_service_handlers(#{message => Message})}.

handle_call(_Request, _From, Message) ->
  {reply, ok, Message}.

handle_cast(serve, #{message := Message} = State) ->
  lager:info("Serve message ~p", [Message]),
  Result = case erlang:apply(wok_config:conf([wok, messages, handler], ?DEFAULT_MESSAGE_HANDLER), parse, [Message]) of
             {ok, ParserMessage, _} ->
               #message{to = To} = ParserMessage,
               case maps:get(eutils:to_binary(To), State, undefined) of
                 undefined ->
                   lager:info("No provider found for ~p : ignore message", [To]),
                   noreply;
                 {Module, Function} ->
                   erlang:apply(Module, Function, [Message])
               end;
             {error, Reason} ->
               lager:info("Error parsing message: ~p", [Reason]),
               noreply
           end,
  _ = wok_dispatcher:finish(self(), Result),
  {noreply, State};
handle_cast(_Msg, Message) ->
  {noreply, Message}.

handle_info(_Info, Message) ->
  {noreply, Message}.

terminate(_Reason, _Message) ->
  ok.

code_change(_OldVsn, Message, _Extra) ->
  {ok, Message}.

get_service_handlers(State) ->
  lists:foldl(fun({ServiceName, Handler}, State1) ->
                  maps:put(ServiceName, Handler, State1)
              end, State, wok_config:conf([wok, messages, services], [])).

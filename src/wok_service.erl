% @hidden
-module(wok_service).
-behaviour(gen_server).
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Message) ->
  gen_server:start_link(?MODULE, Message, []).

init({Message, Service}) ->
  lager:info("Start service ~p with message ~p", [Service, Message]),
  {ok, #{service => Service, message => Message}}.

handle_call(_Request, _From, Message) ->
  {reply, ok, Message}.

handle_cast(serve, #{message := #message{to = To} = Message, service := {Module, Function}} = State) ->
  lager:debug("Serve message ~p", [Message]),
  Result = case erlang:apply(Module, Function, [Message]) of
             {reply, Topic, {Dest, Message}} ->
               {reply, Topic, {To, Dest, Message}};
             Other ->
               Other
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


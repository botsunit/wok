% @hidden
-module(wok_service).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/wok.hrl").
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(MessageTransfert) ->
  gen_server:start_link(?MODULE, MessageTransfert, []).

init(#message_transfert{service = Service} = MessageTransfert) ->
  lager:debug("Start service ~p with message transfert ~p", [Service, MessageTransfert]),
  {ok, MessageTransfert}.

handle_call(_Request, _From, Message) ->
  {reply, ok, Message}.

handle_cast(serve, #message_transfert{message = Message,
                                      action = {Module, Function},
                                      service = Service} = State) ->
  lager:debug("Serve message ~p", [Message]),
  WokState = wok_state:state(),
  {Result, WokState1} = case erlang:apply(Module, Function, [Message, WokState]) of
                          {noreply, RestState} ->
                            {noreply, RestState};
                          {reply, Topic, {Dest, Response}, RestState} ->
                            {{reply, Topic, {Service, Dest, Response}}, RestState};
                          {reply, Topic, Response, RestState} ->
                            {{reply, Topic, Response}, RestState};
                          _ ->
                            {noreply, WokState}
                        end,
  State1 = State#message_transfert{result = Result},
  _ = wok_state:state(WokState1),
  _ = wok_dispatcher:finish(self(), State1),
  {noreply, State1};
handle_cast(_Msg, Message) ->
  {noreply, Message}.

handle_info(_Info, Message) ->
  {noreply, Message}.

terminate(_Reason, _Message) ->
  ok.

code_change(_OldVsn, Message, _Extra) ->
  {ok, Message}.


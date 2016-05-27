% @hidden
-module(wok_service).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/wok.hrl").
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(#message_transfert{consume_method = one_for_all} = MessageTransfert) ->
  gen_server:start_link({local, bucs:to_atom(uuid:to_string(uuid:uuid1()))},
                        ?MODULE, MessageTransfert, []);
start_link(#message_transfert{consume_method = one_for_one, service_name = ServiceName} = MessageTransfert) ->
  gen_server:start_link({local, ServiceName},
                        ?MODULE, MessageTransfert, []).

init(#message_transfert{service = Service} = MessageTransfert) ->
  lager:debug("Start service ~p with message transfert ~p", [Service, MessageTransfert]),
  {ok, MessageTransfert}.

handle_call(_Request, _From, Message) ->
  {reply, ok, Message}.

handle_cast(serve, #message_transfert{message = Message,
                                      action = {Module, Function}} = State) ->
  lager:debug("Serve message ~p", [Message]),
  Message1 = try
               erlang:apply(Module, Function, [Message])
             catch
               E:R -> {exception, {E, R}}
             end,
  State1 = State#message_transfert{message = Message1},
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


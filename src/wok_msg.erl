% @hidden
-module(wok_msg).

-include("../include/wok.hrl").

-export([
         new/0
         , set_response/5
         , get_response/1
         , set_noreply/1
         , get_message/1
         , set_message/2
         , get_uuid/1
         , get_from/1
         , get_to/1
         , get_headers/1
         , get_body/1
         , get_global_state/1
         , set_global_state/2
         , get_local_state/1
         , set_local_state/2
         , get_custom_data/1
         , set_custom_data/2
        ]).

-export_type([wok_msg/0]).

-type wok_msg() :: #wok_msg{}.

new() ->
  #wok_msg{}.

set_response(Msg, Topic, From, To, Body) when is_record(Msg, wok_msg) ->
  Msg#wok_msg{response = #wok_msg_resp{
                            reply = true,
                            from = From,
                            to = To,
                            topic = Topic,
                            body = Body}};
set_response(Msg, Topic, From, To, Body) when is_record(Msg, message_transfert) ->
  Msg#message_transfert{message = set_response(get_wok_msg(Msg), Topic, From, To, Body)}.

get_response(#wok_msg{response = #wok_msg_resp{reply = true,
                                               from = From,
                                               to = To,
                                               topic = Topic,
                                               body = Body}}) ->
  {Topic, From, To, Body};
get_response(#wok_msg{response = #wok_msg_resp{reply = false}}) ->
  noreply;
get_response(#message_transfert{message = Message, service = Service}) ->
  case get_response(Message) of
    noreply -> noreply;
    {Topic, undefined, To, Body} -> {Topic, Service, To, Body};
    Other -> Other
  end.

set_noreply(Msg) when is_record(Msg, wok_msg) ->
  Msg#wok_msg{response = #wok_msg_resp{reply = false}}.

get_message(#wok_msg{message = Message}) ->
  Message;
get_message(Msg) when is_record(Msg, message_transfert) ->
  get_message(get_wok_msg(Msg)).

get_uuid(Msg) when is_record(Msg, message_transfert) ->
  get_uuid(get_wok_msg(Msg));
get_uuid(Msg) when is_record(Msg, wok_msg) ->
  get_uuid(get_message(Msg));
get_uuid(Msg) ->
  wok_message_handler:get_uuid(Msg).

get_from(Msg) when is_record(Msg, message_transfert) ->
  get_from(get_wok_msg(Msg));
get_from(Msg) when is_record(Msg, wok_msg) ->
  get_from(get_message(Msg));
get_from(Msg) ->
  wok_message_handler:get_from(Msg).

get_to(Msg) when is_record(Msg, message_transfert) ->
  get_to(get_wok_msg(Msg));
get_to(Msg) when is_record(Msg, wok_msg) ->
  get_to(get_message(Msg));
get_to(Msg) ->
  wok_message_handler:get_to(Msg).

get_headers(Msg) when is_record(Msg, message_transfert) ->
  get_headers(get_wok_msg(Msg));
get_headers(Msg) when is_record(Msg, wok_msg) ->
  get_headers(get_message(Msg));
get_headers(Msg) ->
  wok_message_handler:get_headers(Msg).

get_body(Msg) when is_record(Msg, message_transfert) ->
  get_body(get_wok_msg(Msg));
get_body(Msg) when is_record(Msg, wok_msg) ->
  get_body(get_message(Msg));
get_body(Msg) ->
  wok_message_handler:get_body(Msg).

set_message(Msg, Message) when is_record(Msg, wok_msg) ->
  Msg#wok_msg{message = Message};
set_message(Msg, Message) when is_record(Msg, message_transfert) ->
  Msg#message_transfert{message = set_message(get_wok_msg(Msg), Message)}.

get_global_state(#wok_msg{global_state = State}) ->
  State;
get_global_state(Msg) when is_record(Msg, message_transfert) ->
  get_global_state(get_wok_msg(Msg)).

set_global_state(Msg, State) when is_record(Msg, wok_msg) ->
  Msg#wok_msg{global_state = State};
set_global_state(Msg, State) when is_record(Msg, message_transfert) ->
  Msg#message_transfert{message = set_global_state(get_wok_msg(Msg), State)}.

get_local_state(#wok_msg{local_state = State}) ->
  State;
get_local_state(Msg) when is_record(Msg, message_transfert) ->
  get_local_state(get_wok_msg(Msg)).

set_local_state(Msg, State) when is_record(Msg, wok_msg) ->
  Msg#wok_msg{local_state = State};
set_local_state(Msg, State) when is_record(Msg, message_transfert) ->
  Msg#message_transfert{message = set_local_state(get_wok_msg(Msg), State)}.

get_custom_data(#wok_msg{custom_data = Data}) ->
  Data;
get_custom_data(Msg) when is_record(Msg, message_transfert) ->
  get_custom_data(get_wok_msg(Msg)).

set_custom_data(Msg, Data) when is_record(Msg, wok_msg) ->
  Msg#wok_msg{custom_data = Data};
set_custom_data(Msg, Data) when is_record(Msg, message_transfert) ->
  Msg#message_transfert{message = set_custom_data(get_wok_msg(Msg), Data)}.

get_wok_msg(#message_transfert{message = Msg}) ->
  Msg.


% @hidden
-module(wok_req).
-include("../include/wok.hrl").
-export([
  set_response/2
  , get_response/1
  , set_response_code/2
  , set_response_headers/2
  , set_response_body/2
  , get_cowboy_req/1
  , set_cowboy_req/2
  , get_custom_data/1
  , set_custom_data/2
  , get_global_state/1
  , set_global_state/2
  , get_local_state/1
  , set_local_state/2
  , new/0
  , reply/1
]).
-export_type([wok_req/0]).

-type wok_req() :: #wok_req{}.

set_response(Req, {Code, Headers, Body}) ->
  bucs:pipecall([
                 {fun set_response_code/2, [Req, Code]},
                 {fun set_response_headers/2, [Headers]},
                 {fun set_response_body/2, [Body]}
                ]).

get_response(#wok_req{response = Resp}) ->
  Resp.

set_response_code(#wok_req{response = Resp} = Req, Code) ->
  Req#wok_req{response = Resp#wok_resp{code = Code}}.

set_response_headers(#wok_req{response = Resp} = Req, Headers) ->
  Req#wok_req{response = Resp#wok_resp{headers = Headers}}.

set_response_body(#wok_req{response = Resp} = Req, Body) ->
  Req#wok_req{response = Resp#wok_resp{body = Body}}.

set_cowboy_req(WokReq, CowboyReq) ->
  WokReq#wok_req{request = CowboyReq}.

get_cowboy_req(#wok_req{request = CowboyReq}) ->
  CowboyReq.

get_custom_data(#wok_req{custom_data = CustomData}) ->
  CustomData.

set_custom_data(Req, CustomData) ->
  Req#wok_req{custom_data = CustomData}.

get_global_state(#wok_req{global_state = State}) ->
  State.

set_global_state(Req, State) ->
  Req#wok_req{global_state = State}.

get_local_state(#wok_req{local_state = State}) ->
  State.

set_local_state(Req, State) ->
  Req#wok_req{local_state = State}.

% @doc
% This function returns a new empty wok_req record
% @end
-spec new() -> wok_req().
new() ->
  #wok_req{}.

% @doc
% This function is an interface to cowboy_req:reply for wok_req
% @end
-spec reply(wok_req()) -> term().
reply(#wok_req{response = #wok_resp{code = C, headers = H, body = B}, request = Req}) ->
  cowboy_req:reply(C, H, B, Req).

% @hidden
-module(wok_req).
-include("../include/wok.hrl").
-export([
  '$handle_undefined_function'/2
  , set_response/2
  , get_response/1
  , set_response_code/2
  , set_response_headers/2
  , get_response_headers/1
  , set_response_body/2
  , get_http_req/1
  , set_http_req/2
  , get_custom_data/1
  , set_custom_data/2
  , get_global_state/1
  , set_global_state/2
  , get_local_state/1
  , set_local_state/2
  , get_handler/1
  , set_handler/2
  , new/1
]).
-export_type([wok_req/0]).

-type wok_req() :: #wok_req{}.

-callback reply(wok_req:wok_req()) -> term().
-callback set_cookie(wok_req:wok_req(),
                     iodata(),
                     iodata(),
                     [{max_age, non_neg_integer()}
                      | {domain, binary()}
                      | {path, binary()}
                      | {secure, boolean()}
                      | {http_only, boolean()}]) -> wok_req:wok_req().
-callback get_cookies(wok_req:wok_req()) -> [{binary(), binary()}].
-callback client_ip(wok_req:wok_req()) -> inet:ip_address().
-callback client_port(wok_req:wok_req()) -> inet:port_number().
-callback body(wok_req:wok_req()) -> {ok | more, binary(), wok_req:wok_req()}.
-callback has_body(wok_req:wok_req()) -> boolean().
-callback body_length(wok_req:wok_req()) -> integer().
-callback method(wok_req:wok_req()) -> term().
-callback path(wok_req:wok_req()) -> term().
-callback header(wok_req:wok_req(), binary(), any()) -> binary() | any() | undefined.
-callback headers(wok_req:wok_req()) -> [{binary(), iodata()}].
-callback post_values(wok_req:wok_req()) -> {ok, [{binary(), binary() | true}], wok_req:wok_req()}
                                            | {error, wok_req:wok_req()}.
-callback get_values(wok_req:wok_req()) -> {ok, [{binary(), binary() | true}], wok_req:wok_req()}
                                           | {error, wok_req:wok_req()}.
-callback binding_values(wok_req:wok_req()) -> {ok, [{binary(), binary() | true}], wok_req:wok_req()}
                                               | {error, wok_req:wok_req()}.

'$handle_undefined_function'(Fun, [#wok_req{adapter = Adapter}|_] = Args) ->
   erlang:apply(Adapter, Fun, Args).

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

get_response_headers(#wok_req{response = #wok_resp{headers = Headers}}) ->
  Headers.

set_response_body(#wok_req{response = Resp} = Req, Body) ->
  Req#wok_req{response = Resp#wok_resp{body = Body}}.

set_http_req(WokReq, CowboyReq) ->
  WokReq#wok_req{request = CowboyReq}.

get_http_req(#wok_req{request = CowboyReq}) ->
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

get_handler(#wok_req{handler = Handler}) ->
  Handler.

set_handler(Req, Handler) ->
  Req#wok_req{handler = Handler}.

-spec new(Adapter :: atom()) -> wok_req().
new(Adapter) ->
  #wok_req{adapter = Adapter}.


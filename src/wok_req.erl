-module(wok_req).
-include("../include/wok.hrl").
-export([
  '$handle_undefined_function'/2
]).

'$handle_undefined_function'(Fun, Args) ->
  erlang:apply(cowboy_req, Fun, to_cowboy_req_args(Args)).

to_cowboy_req_args(WokReqArgs) ->
  lists:map(fun
    (#wok_req{req=Req}) -> Req;
    (Arg) -> Arg
  end, WokReqArgs).

  % custom_data => get/replace

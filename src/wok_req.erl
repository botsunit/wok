% @doc
% This modules responds to all cowboy_req functions.
% @end
-module(wok_req).
-include("../include/wok.hrl").
-export([
  '$handle_undefined_function'/2
  , custom_data/1
  , custom_data/2
]).
-export_type([wok_req/0]).

-type wok_req() :: #wok_req{}.

% @hidden
'$handle_undefined_function'(Fun, Args) ->
  erlang:apply(cowboy_req, Fun, to_cowboy_req_args(Args)).

% @doc
% This function returns wok_req's custom data
% @end
-spec custom_data(wok_req()) -> term().
custom_data(#wok_req{custom_data = CustomData}) -> CustomData.

% @doc
% This function sets wok_req's custom data
% @end
-spec custom_data(wok_req(), term()) -> wok_req().
custom_data(WokReq, CustomData) -> WokReq#wok_req{custom_data = CustomData}.

to_cowboy_req_args(WokReqArgs) ->
  lists:map(fun
    (#wok_req{req=Req}) -> Req;
    (Arg) -> Arg
  end, WokReqArgs).

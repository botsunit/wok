-module(wok_request).
-include("../include/wok.hrl").

-export([
  custom_data/1
  , custom_data/2
  , client_ip/1
  , client_port/1
  , body/1
  , method/1
  , param/4
  , param/3
  , param/2
  , params/2
  , params/1
  , path/1
  , header/2
  , header/3
  , headers/1
  , cookies/1
  , cookie/2
  , local_state/1
  , local_state/2
  , global_state/1
  , global_state/2
]).

% @doc
% This function returns wok_req's custom data
% @end
-spec custom_data(wok_req:wok_req()) -> term().
custom_data(Req) ->
  wok_req:get_custom_data(Req).

% @doc
% This function sets wok_req's custom data
% @end
-spec custom_data(wok_req:wok_req(), term()) -> wok_req:wok_req().
custom_data(Req, Data) ->
  wok_req:set_custom_data(Req, Data).

% @doc
% @end
-spec client_ip(wok_req:wok_req()) -> inet:ip_address().
client_ip(Req) ->
  {IP, _} = cowboy_req:peer(wok_req:get_cowboy_req(Req)),
  IP.

% @doc
% @end
-spec client_port(wok_req:wok_req()) -> inet:port_number().
client_port(Req) ->
  {_, Port} = cowboy_req:peer(wok_req:get_cowboy_req(Req)),
  Port.

% @doc
% @end
body(Req) ->
  {Type, Data, CowboyReq} = cowboy_req:body(wok_req:get_cowboy_req(Req)),
  {Type, Data, wok_req:set_cowboy_req(Req, CowboyReq)}.

% @doc
% This function is an implementation of cowboy_req:method/1 for wok_req
% @end
-spec method(wok_req:wok_req()) -> term().
method(Req) ->
  cowboy_req:method(wok_req:get_cowboy_req(Req)).

%% @doc
%% @end
-spec param(wok_req:wok_req(), get | post | bind, term(), term()) -> {ok, term(), wok_req:req()}
                                                                                | {undefined, wok_req:wok_req()}
                                                                                | {error, wok_req:wok_req()}.
param(Req, Type, Name, Default) ->
  case params(Req, Type) of
    {ok, Params, Req1} ->
      case lists:keyfind(Name, 1, Params) of
        {Name, Value} -> {ok, Value, Req1};
        _ ->
          case Default of
            undefined -> {undefined, Req1};
            _ -> {ok, Default, Req1}
          end
      end;
    Error ->
      Error
  end.

-spec param(wok_req:wok_req(), get | post | bind | term(), term()) -> {ok, term(), wok_req:req()}
                                                                      | {undefined, wok_req:wok_req()}
                                                                      | {error, wok_req:wok_req()}.
param(Req, Type, Name) when Type =:= get; Type =:= post; Type =:= bind ->
  param(Req, Type, Name, undefined);
param(Req, Name, Default) ->
  case params(Req) of
    {ok, Params, Req1} ->
      case lists:keyfind(Name, 1, Params) of
        {Name, Value} -> {ok, Value, Req1};
        _ ->
          case Default of
            undefined -> {undefined, Req1};
            _ -> {ok, Default, Req1}
          end
      end;
    Error ->
      Error
  end.

%% @doc
%% @end
-spec param(wok_req:wok_req(), term()) -> {ok, binary(), wok_req:req()}
                                          | {undefined, wok_req:wok_req()}
                                          | {error, wok_req:wok_req()}.
param(Req, Name) ->
  param(Req, Name, undefined).

%% @doc
%% @end
-spec params(wok_req:wok_req(), get | post | bind) -> {ok, list(), wok_req:req()}
                                                      | {error, wok_req:wok_req()}.
params(Req, Type) ->
  case Type of
    get -> get_vals(Req);
    post -> post_vals(Req);
    bind -> binding_vals(Req)
  end.

%% @doc
%% @end
-spec params(wok_req:wok_req()) -> {ok, list(), wok_req:req()}
                                   | {error, wok_req:wok_req()}.
params(Req) ->
  case post_vals(Req) of
    {ok, PostParams, Req1} ->
      {ok, GetParams, Req2} = get_vals(Req1),
      {ok, BindingParams, Req3} = binding_vals(Req2),
      {ok, merge_params_array(PostParams ++ GetParams ++ BindingParams), Req3};
    Error ->
      Error
  end.

% @doc
% This function is an iimplementation of cowboy_req:path/1 for wok_req
% @end
-spec path(wok_req:wok_req()) -> term().
path(Req) ->
  cowboy_req:path(wok_req:get_cowboy_req(Req)).

% @equiv header(Req, Name, undefined)
header(Req, Name) ->
  header(Req, Name, undefined).

% @doc
% @end
-spec header(wok_req:wok_req(), binary(), any()) -> binary() | any() | undefined.
header(Req, Name, Default) ->
  cowboy_req:header(wok_req:get_cowboy_req(Req), Name, Default).

-spec headers(wok_req:wok_req()) -> [{binary(), iodata()}].
headers(Req) ->
  cowboy_req:headers(wok_req:get_cowboy_req(Req)).

% @doc
% @end
-spec cookies(wok_req:wok_req()) -> [{binary(), binary()}].
cookies(Req) ->
  cowboy_req:parse_cookies(wok_req:get_cowboy_req(Req)).

% @doc
% @end
-spec cookie(wok_req:wok_req(), binary()) -> binary() | undefined.
cookie(Req, Name) ->
  buclists:keyfind(Name, 1, cookies(Req), undefined).

% @doc
% This function get local_state of wok req
% @end
-spec local_state(wok_req:wok_req()) -> term().
local_state(WokReq) ->
  wok_req:get_local_state(WokReq).

% @doc
% This function set local_state of wok req
% @end
-spec local_state(wok_req:wok_req(), term()) -> wok_req:wok_req().
local_state(WokReq, LocalState) ->
  wok_req:set_local_state(WokReq, LocalState).

% @doc
% This function get global_state of wok req
% @end
-spec global_state(wok_req:wok_req()) -> term().
global_state(WokReq) ->
  wok_req:get_global_state(WokReq).

% @doc
% This function set global_state of wok req
% @end
-spec global_state(wok_req:wok_req(), term()) -> wok_req:wok_req().
global_state(WokReq, GlobalState) ->
  wok_req:set_global_state(WokReq, GlobalState).


% Private

post_vals(Req) ->
  case cowboy_req:body_qs(wok_req:get_cowboy_req(Req)) of
    {ok, List, CowboyReq} -> {ok, merge_params_array(List), wok_req:set_cowboy_req(Req, CowboyReq)};
    {_, CowboyReq} -> {error, wok_req:set_cowboy_req(Req, CowboyReq)}
  end.

get_vals(Req) ->
  {ok, merge_params_array(cowboy_req:parse_qs(wok_req:get_cowboy_req(Req))), Req}.

binding_vals(Req) ->
  {ok, merge_params_array(cowboy_req:bindings(wok_req:get_cowboy_req(Req))), Req}.

merge_params_array(Params) ->
  lists:foldl(fun({KeyRaw, Value}, Acc) ->
                  Key = bucs:to_list(KeyRaw),
                  RealKey = case re:run(Key, "([^\\[]*)\\[[^\\]]*\\]$",[{capture,[1],list}]) of
                              {match, [Key1]} -> eutils:to_binary(Key1);
                              nomatch -> Key
                            end,
                  case lists:keyfind(RealKey, 1, Acc) of
                    {RealKey, CurrentValue} when is_list(CurrentValue) ->
                      lists:keyreplace(RealKey, 1, Acc, {RealKey, lists:flatten([value_to_list(Value)|CurrentValue])});
                    {RealKey, CurrentValue} ->
                      lists:keyreplace(RealKey, 1, Acc, {RealKey, lists:flatten([value_to_list(Value), CurrentValue])});
                    false ->
                      [{RealKey, value_to_list(Value)}|Acc]
                  end
              end, [], Params).

value_to_list(Value) ->
  case re:run(Value, "\\[([^\\]]*)]$",[{capture,[1],list}]) of
    {match, [Value1]} -> [eutils:to_binary(X) || X <- string:tokens(eutils:to_string(Value1), ",")];
    nomatch -> Value
  end.


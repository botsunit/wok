-module(wok_request).
-include("../include/wok.hrl").

-export([
  custom_data/1
  , custom_data/2
  , client_ip/1
  , client_port/1
  , body/1
  , param/3
  , param/2
  , params/2
  , params/1
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

%% @doc
%% @end
-spec param(wok_req:wok_req(), get | post | bind, string()) -> {ok, string(), wok_req:req()}
                                                               | {undefined, wok_req:wok_req()}
                                                               | {error, wok_req:wok_req()}.
param(Req, Type, Name) ->
  case params(Req, Type) of
    {ok, Params, Req1} ->
      case lists:keyfind(Name, 1, Params) of
        {Name, Value} -> {ok, Value, Req1};
        _ -> {undefined, Req1}
      end;
    Error ->
      Error
  end.

%% @doc
%% @end
-spec param(wok_req:wok_req(), string()) -> {ok, string(), wok_req:req()}
                                            | {undefined, wok_req:wok_req()}
                                            | {error, wok_req:wok_req()}.
param(Req, Name) ->
  case params(Req) of
    {ok, Params, Req1} ->
      case lists:keyfind(Name, 1, Params) of
        {Name, Value} -> {ok, Value, Req1};
        _ -> {undefined, Req1}
      end;
    Error ->
      Error
  end.

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
  lists:foldl(fun({Key, Value}, Acc) ->
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


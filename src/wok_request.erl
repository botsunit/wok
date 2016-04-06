-module(wok_request).

-export([
  custom_data/1
  , custom_data/2
  , custom_data/3
  , client_ip/1
  , client_port/1
  , body/1
  , has_body/1
  , body_length/1
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
  , handler/1
  , file/1
  , file/3
]).

% @doc
% This function returns wok_req's custom data
% @end
-spec custom_data(wok_req:wok_req()) -> term().
custom_data(Req) ->
  wok_req:get_custom_data(Req).

% @doc
% Return the value for the <tt>Key</tt> in wok_req's custom data
% @end
-spec custom_data(Req :: wok_req:wok_req(), Key :: atom()) -> any().
custom_data(Req, Key) when is_atom(Key) ->
  #{Key := Data} = wok_req:get_custom_data(Req),
  Data.

% @doc
% Set the <tt>Value</tt> for the <tt>Key</tt> in wok_req's custom data.
%
% Return <tt>{ok, Req2}</tt> if the <tt>Key</tt> does not exist in custom data,
% or <tt>{ok, OldData, Req2}</tt> if <tt>Key</tt> already exist in custom data.
% @end
-spec custom_data(Req :: wok_req:wok_req(), Key :: atom(), Value :: any()) ->
  {ok, wok_req:wok_req()}
  | {ok, wok_req:wok_req(), any()}.
custom_data(Req, Key, Value) when is_atom(Key) ->
  case wok_req:get_custom_data(Req) of
    #{Key := Old} = CustomData ->
      {ok, Old, wok_req:set_custom_data(Req, maps:put(Key, Value, CustomData))};
    CustomData ->
      {ok, wok_req:set_custom_data(Req, maps:put(Key, Value, CustomData))}
  end.

% @doc
% @end
-spec client_ip(wok_req:wok_req()) -> inet:ip_address().
client_ip(Req) ->
  wok_req:client_ip(Req).

% @doc
% @end
-spec client_port(wok_req:wok_req()) -> inet:port_number().
client_port(Req) ->
  wok_req:client_port(Req).

% @doc
% @end
-spec body(wok_req:wok_req()) -> {ok | more, binary(), wok_req:wok_req()}.
body(Req) ->
  wok_req:body(Req).

% @doc
% @end
-spec has_body(wok_req:wok_req()) -> boolean().
has_body(Req) ->
  wok_req:has_body(Req).

% @doc
% @end
-spec body_length(wok_req:wok_req()) -> integer().
body_length(Req) ->
  wok_req:body_length(Req).

% @doc
% @end
-spec method(wok_req:wok_req()) -> term().
method(Req) ->
  wok_req:method(Req).

%% @doc
%% @end
-spec param(wok_req:wok_req(), get | post | bind, string() | binary() | atom(), term()) -> {ok, term(), wok_req:req()}
                                                                                           | {undefined, wok_req:wok_req()}
                                                                                           | {error, wok_req:wok_req()}.
param(Req, Type, Name, Default) ->
  param2(params(Req, Type), Name, Default).

-spec param(wok_req:wok_req(), get | post | bind | string() | binary() | atom(), term() | string() | binary() | atom()) -> {ok, term(), wok_req:req()}
                                                                                                                           | {undefined, wok_req:wok_req()}
                                                                                                                           | {error, wok_req:wok_req()}.
param(Req, Type, Name) when Type =:= get; Type =:= post; Type =:= bind ->
  param(Req, Type, Name, undefined);
param(Req, Name, Default) ->
  param2(params(Req), Name, Default).

param2(Params, Name, Default) ->
  RealName = bucs:to_binary(Name),
  case Params of
    {ok, ParamsList, Req1} ->
      case lists:keyfind(RealName, 1, ParamsList) of
        {RealName, Value} -> {ok, Value, Req1};
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
% @end
-spec path(wok_req:wok_req()) -> term().
path(Req) ->
  wok_req:path(Req).

% @equiv header(Req, Name, undefined)
header(Req, Name) ->
  header(Req, Name, undefined).

% @doc
% @end
-spec header(wok_req:wok_req(), binary(), any()) -> binary() | any() | undefined.
header(Req, Name, Default) ->
  wok_req:header(Req, Name, Default).

-spec headers(wok_req:wok_req()) -> [{binary(), iodata()}].
headers(Req) ->
  wok_req:headers(Req).

% @doc
% @end
-spec cookies(wok_req:wok_req()) -> [{binary(), binary()}].
cookies(Req) ->
  wok_req:get_cookies(Req).

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

% @doc
% Get the handler reference
% @end
-spec handler(wok_req:wok_req()) -> pid().
handler(WokReq) ->
  wok_req:get_handler(WokReq).

% @doc
% @end
-spec file(wok_req:wok_req()) -> {ok, binary(), binary(), binary(), wok_req:wok_req()}
                                 | {no_file, wok_req:wok_req()}.
file(WokReq) ->
  wok_req:get_file(WokReq).

% @doc
% @end
-type get_file_callback() :: fun((binary(), binary(), binary(), term()) ->{ok, term()} | {error, term(), term()}).
-spec file(wok_req:wok_req(), get_file_callback(), term()) -> {ok, term(), wok_req:wok_req()}
                                                              | {error, term(), term(), wok_req:wok_req()}
                                                              | {no_file, term(), wok_req:wok_req()}.
file(WokReq, Fun, Acc) ->
  wok_req:get_file(WokReq, Fun, Acc).

% Private

post_vals(Req) ->
  case wok_req:post_values(Req) of
    {ok, List, Req1} -> {ok, merge_params_array(List), Req1};
    Other -> Other
  end.

get_vals(Req) ->
  case wok_req:get_values(Req) of
    {ok, List, Req1} -> {ok, merge_params_array(List), Req1};
    Other -> Other
  end.

binding_vals(Req) ->
  case wok_req:binding_values(Req) of
    {ok, List, Req1} -> {ok, merge_params_array(List), Req1};
    Other -> Other
  end.

merge_params_array(Params) ->
  lists:foldl(fun({KeyRaw, Value}, Acc) ->
                  Key = bucs:to_string(KeyRaw),
                  RealKey = case re:run(Key, "([^\\[]*)\\[[^\\]]*\\]$",[{capture,[1],list}]) of
                              {match, [Key1]} -> bucs:to_binary(Key1);
                              nomatch -> bucs:to_binary(Key)
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
    {match, [Value1]} -> [bucs:to_binary(X) || X <- string:tokens(bucs:to_string(Value1), ",")];
    nomatch -> Value
  end.


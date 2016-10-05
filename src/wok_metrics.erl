% @hidden
-module(wok_metrics).

-export([
         init_controler/2
         , controler_duration/3
        ]).

init_controler(Module, Function) ->
  metrics:new(gauge, controler_metric(Module, Function, <<"duration">>)).

controler_duration(Module, Function, Duration) ->
  metrics:update(controler_metric(Module, Function, <<"duration">>), Duration).

controler_metric(Module, Function, Ext) ->
  Prefix = case doteki:get_as_binary([metrics, metrics_prefix], <<>>) of
    <<>> -> <<>>;
    Other -> <<Other/binary, ".">>
  end,
  bucs:to_string(<<Prefix/binary,
                   "wok.controler.",
                   (bucs:to_binary(Module))/binary,
                   ".",
                   (bucs:to_binary(Function))/binary,
                   ".", Ext/binary>>).

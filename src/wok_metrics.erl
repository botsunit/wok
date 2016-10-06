% @hidden
-module(wok_metrics).

-export([
         init_controler/2
         , controler_duration/3

         , ensure_metrics_mod_started/0
        ]).

ensure_metrics_mod_started() ->
  ensure_metrics_mod_started(metrics:backend()).
ensure_metrics_mod_started(metrics_folsom) ->
  application:ensure_all_started(folsom);
ensure_metrics_mod_started(metrics_exometer) ->
  application:ensure_all_started(exometer);
ensure_metrics_mod_started(metrics_grapherl) ->
  application:ensure_all_started(grapherl);
ensure_metrics_mod_started(_) ->
  ok.

init_controler(Module, Function) ->
  ensure_metrics_mod_started(),
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

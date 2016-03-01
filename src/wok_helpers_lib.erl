% @hidden
-module(wok_helpers_lib).

% Mandatory
-export([filters/0, tags/0]).

% Filters
-export([foo/2]).

% Tags
-export([static/1]).

filters() -> [foo].
tags() -> [static].

foo(Value, V) ->
  "foo:" ++ bucs:to_string(Value) ++ ":" ++ bucs:to_string(V).

static(Data) ->
  Static = wok_state:static(route),
  Static1 = if
              is_binary(Static) ->
                lists:flatten(binary_to_list(Static));
              true ->
                Static
            end,
  case lists:reverse(Static1) of
    "/" ++ _ ->
      Static1 ++ bucs:to_string(Data);
    "" ->
      bucs:to_string(Data);
    _ ->
      Static1 ++ "/" ++ bucs:to_string(Data)
  end.



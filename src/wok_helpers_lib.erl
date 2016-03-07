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

static([Data]) ->
  wok_routes:static(Data).


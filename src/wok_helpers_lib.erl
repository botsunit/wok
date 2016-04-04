% @hidden
-module(wok_helpers_lib).

% Mandatory
-export([filters/0, tags/0]).

% Filters
-export([foo/2]).

% Tags
-export([
         static/1
         , route/1
        ]).

filters() -> [foo].
tags() -> [static, route].

foo(Value, V) ->
  "foo:" ++ bucs:to_string(Value) ++ ":" ++ bucs:to_string(V).

static([Data]) ->
  wok_routes:static(Data).

route([Module, Function]) ->
  wok_routes:path(bucs:to_atom(Module),
                  bucs:to_atom(Function));
route([Verb, Module, Function]) ->
  wok_routes:path(
    bucs:to_atom(Verb),
    bucs:to_atom(Module),
    bucs:to_atom(Function)).


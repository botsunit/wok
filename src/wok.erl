-module(wok).

-export([start/0]).
-export([state/0, state/1]).

% @hidden
start() ->
  application:ensure_all_started(?MODULE).

% @hidden
-spec state() -> term().
state() ->
  wok_state:state().

% @hidden
-spec state(State :: term()) -> ok.
state(State) ->
  wok_state:state(State).


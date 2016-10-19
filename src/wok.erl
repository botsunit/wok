-module(wok).

-export([start/0]).
-export([state/0, state/1]).

% @doc
% Start Wok
% @end
-spec start() -> ok.
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


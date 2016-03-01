-module(wok).
-include("../include/wok.hrl").

-export([start/0]).
-export([provide/5, provide/4, provide/2]).
-export([state/0, state/1]).

% @hidden
start() ->
  application:ensure_all_started(?MODULE).

% @doc
% Send a message
% @end
-spec provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
              From :: binary(),
              To :: binary(),
              Body :: term()) -> {ok, term()} | {error, term()}.
provide(Topic, From, To, Body) ->
  wok_producer:provide(Topic, From, To, Body).

% @doc
% Send a message
% @end
-spec provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
              From :: binary(),
              To :: binary(),
              Body :: term(),
              Options :: map()) -> {ok, term()} | {error, term()}.
provide(Topic, From, To, Body, Options) ->
  wok_producer:provide(Topic,  From, To, Body, Options).

% @doc
% Send a message
% @end
-spec provide(Topic :: binary() | list() | atom() | {binary() | list() | atom(), integer()},
              Message :: binary()) -> {ok, term()} | {error, term()}.
provide(Topic, Message) ->
  wok_producer:provide(Topic, Message).

% @hidden
-spec state() -> term().
state() ->
  wok_state:state().

% @hidden
-spec state(State :: term()) -> ok.
state(State) ->
  wok_state:state(State).


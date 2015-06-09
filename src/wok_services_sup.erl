-module(wok_services_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/1]).
-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, temporary, 2000, Type, [I]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(Message) ->
  supervisor:start_child(?MODULE, [Message]).

init([]) ->
  {ok, { {simple_one_for_one, 0, 1}, [?CHILD(wok_service, worker)]} }.


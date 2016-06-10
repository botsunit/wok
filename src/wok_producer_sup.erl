% @hidden
-module(wok_producer_sup).
-behaviour(supervisor).

-export([
         start_link/0
         , stop_child/1
         , start_child/1
        ]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop_child(Pid) when is_pid(Pid) ->
  supervisor:terminate_child(?MODULE, Pid).

start_child(Args) ->
  case supervisor:start_child(?MODULE, Args) of
    {ok, Child, _} -> {ok, Child};
    Other -> Other
  end.

init([]) ->
  {ok, {
     #{strategy => one_for_one,
       intensity => 1,
       period => 5},
     [
      #{id => wok_producer_async_sup,
        start => {wok_producer_async_sup, start_link, []},
        type => supervisor,
        shutdown => 5000},
      #{id => wok_producer_srv,
        start => {wok_producer_srv, start_link, []},
        type => worker,
        shutdown => 5000}
     ]
    }}.


% @hidden
-module(wok_topics_sup).
-compile([{parse_transform, lager_transform}]).
-behaviour(supervisor).

-export([start_link/0, start_child/1, terminate_child/1, which_children/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_child(TopicParams) ->
  supervisor:start_child(?MODULE, TopicParams).

terminate_child(Child) ->
  supervisor:terminate_child(?MODULE, Child).

which_children() ->
  [PID || {PID, _, _, _} <- supervisor:which_children(?MODULE)].

init([]) ->
  SupFlags = #{strategy => simple_one_for_one,
               intensity => 0,
               period => 1},
  ChildSpecs = [#{id => wok_topic,
                  start => {wok_topic, start_link, []},
                  shutdown => 2000}],
  {ok, {SupFlags, ChildSpecs}}.


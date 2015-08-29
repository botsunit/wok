% @hidden
-module(wok_state).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([state/0, state/1]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

state() ->
  gen_server:call(?SERVER, state).

state(State) ->
  gen_server:cast(?SERVER, {state, State}).

init(_) ->
  case wok_config:conf([wok, initializer]) of
    [{Module, Args}] ->
      erlang:apply(Module, init, [Args]);
    _ ->
      {ok, nostate}
  end.

handle_call(state, _, State) ->
  {reply, State, State};
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({state, State}, _) ->
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


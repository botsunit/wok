% @hidden
-module(wok_state).
-behaviour(gen_server).
-compile([{parse_transform, lager_transform}]).
-define(SERVER, ?MODULE).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).
-export([state/0, state/1, static/1]).

start_link(Static) ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [Static], []).

state() ->
  gen_server:call(?SERVER, state).

state(State) ->
  gen_server:cast(?SERVER, {state, State}).

static(path) ->
  gen_server:call(?SERVER, static_path);
static(route) ->
  gen_server:call(?SERVER, static_route).

init([Static]) ->
  case doteki:get_env([wok, initializer]) of
    [{Module, Args}] ->
      case erlang:apply(Module, init, [Args]) of
        {ok, State} ->
          {ok, Static#{state => State}};
        Other ->
          Other
      end;
    _ ->
      {ok, Static#{state => nostate}}
  end.

handle_call(Request, _From, State) ->
  {reply, maps:get(Request, State, ok), State}.

handle_cast({state, GlobalState}, State) ->
  {noreply, State#{state := GlobalState}};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, #{state := GlobalState}) ->
  case doteki:get_env([wok, initializer]) of
    [{Module, _}] ->
      _ = bucs:call(Module, terminate, [Reason, GlobalState]),
      ok;
    _ ->
      ok
  end.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


% @hidden
-module(wok_async_producer_state).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/wok.hrl").

-export([
         start_link/0,
         put/1,
         get/0
        ]).
%%
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).
-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

put(Topics) ->
  gen_server:call(?SERVER, {put, Topics}).
get() ->
  gen_server:call(?SERVER, {get}).

init([]) ->
  lager:info("Wok producer state started"),
  {ok, #{topics => #{paused => [], started => []}}}.

handle_call({put, Topics}, _From, State) ->
  {reply, ok, State#{topics => Topics}};
handle_call({get}, _From, State) ->
  {reply, maps:get(topics, State), State}.

handle_cast(_, State) ->
  {reply, ok, State}.

handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.





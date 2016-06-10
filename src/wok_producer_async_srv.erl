% @hidden
-module(wok_producer_async_srv).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {
          topic,
          partition,
          status
         }).

start_link(Args) ->
  gen_server:start_link(?MODULE, Args, []).

% @hidden
init([Topic, Partition]) ->
  lager:info("Producer for ~p#~p started", [Topic, Partition]),
  {ok, #state{
          topic = Topic,
          partition = Partition,
          status = started
         }}.

% @hidden
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

% @hidden
handle_cast(_Msg, State) ->
  {noreply, State}.

% @hidden
handle_info(_Info, State) ->
  {noreply, State}.

% @hidden
terminate(_Reason, _State) ->
  ok.

% @hidden
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


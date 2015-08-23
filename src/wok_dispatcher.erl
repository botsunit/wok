% @hidden
-module(wok_dispatcher).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0, handle/1]).
-export([finish/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

handle(Message) ->
  gen_server:call(?SERVER, {handle, Message}).

finish(Child, Result) ->
  gen_server:cast(?SERVER, {terminate, Child, Result}).

%% ------------------------------------------------------------------

init(_Args) ->
  {ok, []}.

handle_call({handle, Message}, _From, State) ->
  % TODO: Queue
  case wok_services_sup:start_child(Message) of
    {ok, Child} ->
      {reply, gen_server:cast(Child, serve), State};
    {ok, Child, _} ->
      {reply, gen_server:cast(Child, serve), State};
    {queue, Message} ->
      % TODO
      lager:info("Queue message..."),
      {reply, ok, State};
    {error, Reason} ->
      lager:info("Faild to start service : ~p", [Reason]),
      {reply, error, State}
  end;
handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast({terminate, Child, Result}, State) ->
  case Result of
    noreply -> 
      ok;
    {reply, Topic, Message} ->
      % TODO
      lager:info("Reply @ ~p =================> ~p", [Topic, Message]);
    _ ->
      lager:error("Invalid response : ~p", [Result]),
      ignore
  end,
  _ = case wok_services_sup:terminate_child(Child) of
        ok -> ok;
        _ ->
          lager:error("Faild to stop service #~p", [Child])
      end,
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


% @hidden
-module(wok_service).
-behaviour(gen_server).
-include_lib("wok_message/include/wok_message.hrl").

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link(Message) ->
  gen_server:start_link(?MODULE, [Message], []).

init(Args) ->
  lager:info("Start service with message ~p", [Args]),
  {ok, Args}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(serve, State) ->
  lager:info("Serve message ~p", [State]),
  #message{to = To} = Message = wok_message:parse(State),
  %lager:info("~p", [Message]),
  % TODO: get provider from message
  % TODO: run provider for message
  _ = timer:sleep(10000),
  Result = ok,
  % END
  _ = wok_dispatcher:finish(self(), Result),
  {noreply, State};
handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


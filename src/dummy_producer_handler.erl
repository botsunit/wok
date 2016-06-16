% @hidden
-module(dummy_producer_handler).
-behaviour(wok_producer).

%% API
-export([start_link/0]).
-export([store/3, messages/3, response/2]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).

store(Topic, Partition, Message) ->
  gen_server:call(?MODULE, {store, Topic, Partition, Message}).

messages(Topic, Partition, NumMessage) ->
  gen_server:call(?MODULE, {messages, Topic, Partition, NumMessage}).

response(MessageID, Response) ->
  gen_server:call(?MODULE, {response, MessageID, Response}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

% @hidden
init([]) ->
  {ok, []}.

% @hidden
handle_call({store, Topic, Partition, Message}, _From, State) ->
  State1 = case lists:keyfind({Topic, Partition}, 1, State) of
             {{Topic, Partition}, List} ->
               lists:keyreplace({Topic, Partition}, 1, State, {{Topic, Partition}, List ++ [Message]});
             false ->
               [{{Topic, Partition}, [Message]}|State]
           end,
  {reply, ok, State1};
handle_call({messages, Topic, Partition, NumMessage}, _From, State) ->
  Reply = case lists:keyfind({Topic, Partition}, 1, State) of
            {{Topic, Partition}, List} ->
              [{base64:encode(term_to_binary({Topic, Partition, M})), Topic, Partition, M}
               || M <- lists:sublist(List, NumMessage)];
            false ->
              []
          end,
  {reply, Reply, State};
handle_call({response, MessageID, Response}, _From, State) ->
  case Response of
    {error, _} ->
      {reply, exit, State};
    _ ->
      {Topic, Partition, Message} = binary_to_term(base64:decode(MessageID)),
      case lists:keyfind({Topic, Partition}, 1, State) of
        {{Topic, Partition}, List} ->
          {reply, next, lists:keyreplace({Topic, Partition}, 1, State,
                                         {{Topic, Partition}, lists:delete(Message, List)})};
        false ->
          {reply, next, State}
      end
  end;
handle_call(_Message, _From, State) ->
  {reply, ok, State}.
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


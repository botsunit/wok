% @hidden
-module(dummy_producer_handler).
-compile([{parse_transform, lager_transform}]).
-behaviour(wok_producer).

%% API
-export([start_link/0]).
-export([
         store/3,
         messages/2,
         response/2
        ]).

%% gen_server callbacks
-export([init/1
         , handle_call/3
         , handle_cast/2
         , handle_info/2
         , terminate/2
         , code_change/3]).

store(Topic, Partition, Message) ->
  gen_server:call(?MODULE, {store, Topic, Partition, Message}).

messages(Topics, NumMessage) ->
  gen_server:call(?MODULE, {messages, Topics, NumMessage}, infinity).

response(OK, KO) ->
  gen_server:call(?MODULE, {response, OK, KO}).

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
handle_call({messages, Topics, NumMessage}, _From, State) ->
  Reply = get_messages(Topics, State, NumMessage),
  {reply, Reply, State};
handle_call({response, OK, _KO}, _From, State) ->
  {reply, ok, delete_messages(OK, State)};
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

get_messages(Topics, State, NumMessage) ->
  get_messages(Topics, State, NumMessage, []).

get_messages([], _, _, Acc) ->
  Acc;
get_messages([{Topic, Partitions}|Rest], State, NumMessage, Acc) ->
  get_messages(Rest, State, NumMessage,
               Acc ++ get_messages(Topic, Partitions, State, NumMessage, [])).

get_messages(_, [], _, _, Acc) ->
  Acc;
get_messages(Topic, [Partition|Rest], State, NumMessage, Acc) ->
  get_messages(Topic, Rest, State, NumMessage,
               [{base64:encode(term_to_binary({Topic, Partition, M})), Topic, Partition, M} || M <- buclists:keyfind({Topic, Partition}, 1, State, [])] ++ Acc).

delete_messages([], State) ->
  State;
delete_messages([ID|Rest], State) ->
  {Topic, Partition, Message} = binary_to_term(base64:decode(ID)),
  delete_messages(
    Rest,
    case lists:keyfind({Topic, Partition}, 1, State) of
      {{Topic, Partition}, List} ->
        lists:keyreplace({Topic, Partition}, 1, State,
                         {{Topic, Partition}, lists:delete(Message, List)});
      false ->
        State
    end).


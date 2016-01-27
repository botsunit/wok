% @hidden
-module(wok_topics_manager).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-include("../include/wok.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
  terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([]) ->
  erlang:send_after(1000, self(), manage),
  {ok, #{}}.

handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(manage, State) ->
  lager:debug("Manage topics"),
  State1 = case doteki:get_env([wok, messages, topics]) of
             undefined ->
               lager:debug("No topic declared in config"),
               State;
             Topics ->
               manage_topic(Topics, State)
           end,
  erlang:send_after(10000, self(), manage),
  {noreply, State1};
handle_info({fetch, TopicLocalName, Frequency}, State) ->
  State1 = case maps:get(TopicLocalName, State, undefined) of
             undefined ->
               State;
             TopicState ->
               lager:debug("Will fetch ~p : ~p", [TopicLocalName, TopicState]),
               wok_topic:fetch(TopicLocalName),
               Timer = erlang:send_after(Frequency, self(), {fetch, TopicLocalName, Frequency}),
               State#{TopicLocalName => TopicState#{timer => Timer}}
           end,
  {noreply, State1};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

manage_topic(Topics, State) ->
  maps:fold(fun(LocalName, #{name := Name, options := Options} = Info, Acc) ->
                case maps:merge(maps:get(LocalName, State, #{}), Info) of
                  Info ->
                    lager:debug("topic ~p not started.", [LocalName]),
                    start_child(LocalName, Name, Options, Info, Acc);
                  #{name := Name, options := Options, pid := PID, timer := _ } = Info1 ->
                    lager:debug("topic ~p already started (PID: ~p)", [LocalName, PID]),
                    maps:put(LocalName, Info1, Acc);
                  #{pid := PID, timer := Timer} ->
                    lager:debug("topic ~p (PID: ~p) must be restarted", [LocalName, PID]),
                    _ = erlang:cancel_timer(Timer, [{async, true}, {info, false}]),
                    case wok_topics_sup:terminate_child(PID) of
                      ok ->
                        start_child(LocalName, Name, Options, Info, Acc);
                      {error, Reason} ->
                        lager:debug("Terminate child (PID: ~p) faild: ~p", [PID, Reason]),
                        Acc
                    end
                end
            end, #{}, topic(Topics, #{})).

topic([], Result) ->
  Result;
topic([{Name, Options}|Rest], Result) ->
  topic([{Name, one_for_all, Options}|Rest], Result);
topic([{Name, one_for_all, Options}|Rest], Result) ->
  topic(Rest, Result#{bucs:to_atom(Name) => #{
                   name => Name,
                   options => [{consume, one_for_all}|Options]
                  }});
topic([{Name, one_for_one, Options}|Rest], Result) ->
  #{Name := Topic} = kafe:topics(),
  Result1 = lists:foldl(
              fun(P, Acc) ->
                  Acc#{
                    bucs:to_atom(<<Name/binary, "_", (bucs:to_binary(P))/binary>>) => #{
                                                                           name => Name,
                                                                           options => [{partition, P},
                                                                                       {consume, one_for_one}|
                                                                                       Options]
                                                                          }}
              end, Result, maps:keys(Topic)),
  topic(Rest, Result1).

start_child(LocalName, Name, Options, Info, State) ->
  Frequency = buclists:keyfind(fetch_frequency, 1, Options, ?DEFAULT_FETCH_FREQUENCY),
  case wok_topics_sup:start_child([Name, Options]) of
    {ok, NPID} ->
      Timer = erlang:send_after(Frequency, self(), {fetch, LocalName, Frequency}),
      maps:put(LocalName, Info#{pid => NPID, timer => Timer}, State);
    {ok, NPID, _} ->
      Timer = erlang:send_after(Frequency, self(), {fetch, LocalName, Frequency}),
      maps:put(LocalName, Info#{pid => NPID, timer => Timer}, State);
    _ ->
      State
  end.

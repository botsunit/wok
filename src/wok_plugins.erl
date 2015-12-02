% @hidden
-module(wok_plugins).
-compile([{parse_transform, lager_transform}]).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-export([start_link/0]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init(_) ->
  {Plugins, Confs} = lists:foldl(fun
                                   ({Name, Calls, Opts}, {PluginsAcc, ConfAcc}) ->
                                     case erlang:apply(Name, init, [Opts]) of
                                       {ok, Args} ->
                                         lager:info("Plugin ~p started", [Name]),
                                         {[Name|PluginsAcc],
                                          maps:put(Name, #{state => Args, calls => Calls}, ConfAcc)};
                                       {stop, Reason} ->
                                         lager:debug("Plugin ~p stop: ~p", [Name, Reason]),
                                         {PluginsAcc, ConfAcc}
                                     end;
                                   ({Name, Calls}, {PluginsAcc, ConfAcc}) ->
                                     {[Name|PluginsAcc],
                                      maps:put(Name, #{state => undefined, calls => Calls}, ConfAcc)}
                                 end, {[], #{}}, wok_config:conf([wok, plugins], [])),
  State = #{plugins => lists:reverse(Plugins),
            confs => Confs},
  lists:foreach(fun(Plugin) ->
                    PluginCalls = get_plugin_calls(Plugin, State),
                    lists:foreach(fun(Call) ->
                                      start_plugin(Plugin, Call)
                                  end, PluginCalls)
                end, Plugins),
  {ok, State}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info({run, Name, {Fun, _} = Call}, State) ->
  PluginState = get_plugin_state(Name, State),
  lager:debug("Run plugin ~p:~p(~p)", [Name, Fun, PluginState]),
  State2 = case erlang:apply(Name, Fun, [PluginState]) of
             {ok, PluginState2} ->
               _ = start_plugin(Name, Call),
               update_plugin_state(Name, PluginState2, State);
             {send, Topic, Message, PluginState3} ->
               _ = start_plugin(Name, Call),
               _ = wok:provide(Topic, Message),
               update_plugin_state(Name, PluginState3, State);
             {stop, Reason} ->
               lager:debug("Plugin ~p:~p stop: ~p", [Name, Fun, Reason]),
               State
           end,
  {noreply, State2};
handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

get_plugin_calls(Plugin, #{confs := Confs}) ->
  maps:get(calls, maps:get(Plugin, Confs)).

get_plugin_state(Plugin, #{confs := Confs}) ->
  maps:get(state, maps:get(Plugin, Confs)).

update_plugin_state(Name, PluginState, #{confs := Confs} = State) ->
  PluginInfo = maps:get(Name, Confs), 
  State#{confs => maps:put(Name, PluginInfo#{state => PluginState}, Confs)}.

start_plugin(Name, {Fun, Freq}) ->
  case buctimer:next(Freq) of
    {ok, _, Sec} ->
      {ok, erlang:send_after(Sec * 1000, self(), {run, Name, {Fun, Freq}})};
    stop ->
      lager:info("Plugin ~p:~p stopped", [Name, Fun]),
      stop;
    {error, Reason} ->
      lager:error("Plugin ~p:~p, configuration error: ~p", [Name, Fun, Reason]),
      {error, Reason}
  end.


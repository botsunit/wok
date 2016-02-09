-module(wok_initializer).

-callback init(Args :: list()) ->
  {ok, State :: term()}
  | {ok, State :: term(), Timeout :: integer()}
  | {ok, State :: term(), hibernate}
  | {stop, Reason :: term()}
  | ignore.

-callback terminate(Reason :: term(), State :: term()) -> ok.


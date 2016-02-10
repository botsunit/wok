-module(wok_initializer).

-callback init(Args :: list()) ->
  {ok, State :: term()}
  | {ok, State :: term(), Timeout :: integer()}
  | {ok, State :: term(), hibernate}
  | {stop, Reason :: term()}
  | ignore.

% This function is optional
% -callback terminate(Reason :: term(), State :: term()) -> ok.


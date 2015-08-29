-module(wok_provider_handler).
-include_lib("wok_message_handler/include/wok_message_handler.hrl").

-callback my_service(Message :: wok_message_handler:message()) -> 
  {noreply, State ::term()}
  | {reply, Topic :: binary(), {From :: binary(), To :: binary(), Body :: binary()}, State :: term()} 
  | {reply, Topic :: binary(), {To :: binary(), Body :: binary()}, State :: term()}
  | {reply, Topic :: binary(), Message :: binary(), State :: term()}.


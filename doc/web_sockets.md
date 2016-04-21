# Web Sockets

how to configure wok to use web socket 

1/ add a route in wok config

```erlang
[  
  {wok, [  
    ...  
    {rest, [  
      ...  
      {routes, [  
        {'WS', "/ws_endpoint", my_web_socket_rest_handler},  
      ]}  
    ]}  
  ]}  
].  

```

```elixir
config :wok,  
  ...  
  rest: [  
    ...  
    routes: [  
      {:WS, '/ws_endpoint', MyWebSocketController},  
    ]  
  ]  
```

2/ create the controller with required methods on server side

```erlang  
% @hidden  
-module(my_web_socket_rest_handler).  

-export([ws_init/1, ws_handle/2, ws_info/2]).  

ws_init(Req) ->  
  gproc:reg({p, l, my_room}, ""),  
  io:format("Process ~p inited",[self()]),  
  {ok, Req}.  

ws_handle({text, Msg}, Req) ->  
  io:format("Process ~p will send ~p",[self(), Msg]),  
  {ok, Req};  
ws_handle(_, Req) ->  
  {ok, Req}.  

ws_info(Msg, Req) ->  
  io:format("Process ~p has received ~p",[self(), Msg]),  
  {reply, {text, Msg}, Req}.  
```

```elixir  
defmodule MyWebSocketController do  

  def ws_init(request) do  
    Process.register(self, :wsocket_process_xxx) #you can use something more powerfull to register process with a uniq id like gproc
    {:ok, request}  
  end  

  def ws_handle(data, request) do
    IO.inspect("called each time a message is sent from browser on the socket")  
    {:ok, request}  
  end  

  def ws_info(data, request) do  
    IO.inspect("called each time a message is sent to the registered process in ws_init on server side")  
    IO.inspect("send 'data' to browser on websocket")  
    {:reply, [text: data], request}  
  end  
end  
```

3/ create your web socket client in browser/javascript

```
var myWebSocket = new WebSocket("ws://localhost/ws_endpoint");  
realTimeChannel.onmessage = function (event) {  
  console.log(event); // data received from ws_info function  
};  
  
function sendToServer(data){  
  myWebSocket.send(data);  
}  
  
sendToServer("hello world");  
```
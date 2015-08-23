

# Scalezen framework to create bots #

Copyright (c) 2015 Grégoire Lejeune

__Version:__ 0.0.1

__Authors:__ Gregoire Lejeune ([`gregoire.lejeune@scalezen.com`](mailto:gregoire.lejeune@scalezen.com)).



### Create a bot with wok ###


#### Erlang ####


#### Elixir ####


#### Ruby ####


#### Python ####


#### Javascript ####


### Configuration ###

* `messages :: list()` : [Messages configuration](#messages_conf)

* `rest :: list()` : [REST configuration](#rest_conf)

<a name="messages_conf"></a>

#### Messages configuration ####

* `handler :: atom()` :

* `services :: list()` : [Services configuration](#services_conf)

* `consumer_group :: binary()` :

* `local_queue_name :: binary()` :

* `local_consumer_group :: binary()` :

* `max_services_fork :: integer()` :

* `topics :: list()` : [Topics configuration](#topic_conf)

<a name="rest_conf"></a>

#### REST configuration ####

* `port :: integer()` :

* `ip :: list()` :

* `max_conn :: integer()` :

* `routes :: list()` : [Routes configuration](#routes_conf)

<a name="services_conf"></a>

#### Services configuration ####

```

{<<"my_service">>, {my_service_module, my_service_function}}

```
<a name="topics_conf"></a>

#### Services configuration ####

```

{<<"topic">>, [{fetch_frequency, 5000}, {max_bytes, 10485760}]}

```
<a name="routes_conf"></a>

#### Routes configuration ####

```

{'GET', "/path", {route_module, route_function}}

```


### Dependencies ###

![](wok.deps.png)


### Call tree ###

![](wok.call.png)


### Licence ###

Wok is available for use under the following license, commonly known as the 3-clause (or "modified") BSD license:

Copyright (c) 2015 Grégoire Lejeune

Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:

* Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.

* Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.

* The name of the author may not be used to endorse or promote products derived from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
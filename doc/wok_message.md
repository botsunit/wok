

# Module wok_message #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-message">message()</a> ###


<pre><code>
message() = term()
</code></pre>




### <a name="type-opaque">opaque()</a> ###


<pre><code>
opaque() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_reply-1">async_reply/1</a></td><td>
Set an async response.</td></tr><tr><td valign="top"><a href="#body-1">body/1</a></td><td>
Return the incoming message body.</td></tr><tr><td valign="top"><a href="#body-2">body/2</a></td><td>
Update the incoming message body.</td></tr><tr><td valign="top"><a href="#content-1">content/1</a></td><td>
Return the incoming message as map.</td></tr><tr><td valign="top"><a href="#custom_data-1">custom_data/1</a></td><td>
Return the custon datas for the given message.</td></tr><tr><td valign="top"><a href="#encode_message-4">encode_message/4</a></td><td>
Encode a reponse for an async producer.</td></tr><tr><td valign="top"><a href="#encode_reply-4">encode_reply/4</a></td><td> 
Encore a message for an async producer.</td></tr><tr><td valign="top"><a href="#encode_reply-5">encode_reply/5</a></td><td>
Encore a message for an async producer.</td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td>
Return the incoming message from.</td></tr><tr><td valign="top"><a href="#global_state-1">global_state/1</a></td><td>
Return the global state for the given message.</td></tr><tr><td valign="top"><a href="#headers-1">headers/1</a></td><td>
Return the incoming message headers.</td></tr><tr><td valign="top"><a href="#local_state-1">local_state/1</a></td><td>
Return the local state for the given message.</td></tr><tr><td valign="top"><a href="#noreply-1">noreply/1</a></td><td>
Set a no reply response.</td></tr><tr><td valign="top"><a href="#params-1">params/1</a></td><td>
Return the incoming message params.</td></tr><tr><td valign="top"><a href="#provide-2">provide/2</a></td><td>
Send a message.</td></tr><tr><td valign="top"><a href="#provide-4">provide/4</a></td><td>
Send a message.</td></tr><tr><td valign="top"><a href="#provide-5">provide/5</a></td><td>
Send a message.</td></tr><tr><td valign="top"><a href="#reply-4">reply/4</a></td><td> 
Set the response message.</td></tr><tr><td valign="top"><a href="#reply-5">reply/5</a></td><td>
Set the response message.</td></tr><tr><td valign="top"><a href="#response-1">response/1</a></td><td>
Return the outgoing message as map.</td></tr><tr><td valign="top"><a href="#response_body-1">response_body/1</a></td><td>
Return the outgoing message body.</td></tr><tr><td valign="top"><a href="#response_body-2">response_body/2</a></td><td>
Update the outgoing message body.</td></tr><tr><td valign="top"><a href="#response_from-1">response_from/1</a></td><td>
Return the outgoing message from.</td></tr><tr><td valign="top"><a href="#response_from-2">response_from/2</a></td><td>
Update the outgoing message from.</td></tr><tr><td valign="top"><a href="#response_to-1">response_to/1</a></td><td>
Return the outgoing message to.</td></tr><tr><td valign="top"><a href="#response_to-2">response_to/2</a></td><td>
Update the outgoing message to.</td></tr><tr><td valign="top"><a href="#to-1">to/1</a></td><td>
Return the incoming message to.</td></tr><tr><td valign="top"><a href="#uuid-1">uuid/1</a></td><td>
Return the incoming message UUID.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_reply-1"></a>

### async_reply/1 ###

<pre><code>
async_reply(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; <a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>
</code></pre>
<br />

Set an async response

<a name="body-1"></a>

### body/1 ###

<pre><code>
body(Wok_message::<a href="#type-message">message()</a>) -&gt; binary() | undefined
</code></pre>
<br />

Return the incoming message body

<a name="body-2"></a>

### body/2 ###

<pre><code>
body(Wok_message::<a href="#type-message">message()</a>, Body::term()) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Update the incoming message body

<a name="content-1"></a>

### content/1 ###

<pre><code>
content(Message::<a href="#type-message">message()</a>) -&gt; #{}
</code></pre>
<br />

Return the incoming message as map

<a name="custom_data-1"></a>

### custom_data/1 ###

<pre><code>
custom_data(Wok_message::<a href="#type-message">message()</a>) -&gt; #{}
</code></pre>
<br />

Return the custon datas for the given message

<a name="encode_message-4"></a>

### encode_message/4 ###

<pre><code>
encode_message(Topic::binary() | {Topic::binary(), Partition::integer()} | {Topic::binary(), Key::binary()}, From::binary(), To::binary(), Body::term()) -&gt; {ok, Topic::binary(), Partition::integer(), EncodedMessage::<a href="#type-opaque">opaque()</a>} | {error, term()}
</code></pre>
<br />

Encode a reponse for an async producer

<a name="encode_reply-4"></a>

### encode_reply/4 ###

<pre><code>
encode_reply(Message::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, Topic::binary() | {Topic::binary(), Partition::integer()} | {Topic::binary(), Key::binary()}, To::binary(), Body::term()) -&gt; {ok, Topic::binary(), Partition::integer(), EncodedMessage::<a href="#type-opaque">opaque()</a>} | {error, term()}
</code></pre>
<br />


Encore a message for an async producer

Since the sender is not specified, we will use the recipient of the incoming message

<a name="encode_reply-5"></a>

### encode_reply/5 ###

<pre><code>
encode_reply(Message::<a href="#type-message">message()</a>, Topic::binary() | {Topic::binary(), Partition::integer()} | {Topic::binary(), Key::binary()}, From::binary(), To::binary(), Body::term()) -&gt; {ok, Topic::binary(), Partition::integer(), EncodedMessage::<a href="#type-opaque">opaque()</a>} | {error, term()}
</code></pre>
<br />

Encore a message for an async producer

<a name="from-1"></a>

### from/1 ###

<pre><code>
from(Wok_message::<a href="#type-message">message()</a>) -&gt; binary() | undefined
</code></pre>
<br />

Return the incoming message from

<a name="global_state-1"></a>

### global_state/1 ###

<pre><code>
global_state(Wok_message::<a href="#type-message">message()</a>) -&gt; term() | undefined
</code></pre>
<br />

Return the global state for the given message

<a name="headers-1"></a>

### headers/1 ###

<pre><code>
headers(Wok_message::<a href="#type-message">message()</a>) -&gt; binary() | undefined
</code></pre>
<br />

Return the incoming message headers

<a name="local_state-1"></a>

### local_state/1 ###

<pre><code>
local_state(Wok_message::<a href="#type-message">message()</a>) -&gt; term() | undefined
</code></pre>
<br />

Return the local state for the given message

<a name="noreply-1"></a>

### noreply/1 ###

<pre><code>
noreply(Message::<a href="#type-message">message()</a>) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Set a no reply response

<a name="params-1"></a>

### params/1 ###

<pre><code>
params(Wok_message::<a href="#type-message">message()</a>) -&gt; binary() | undefined
</code></pre>
<br />

Return the incoming message params

<a name="provide-2"></a>

### provide/2 ###

<pre><code>
provide(Topic::binary() | {binary(), integer()} | {binary(), binary()}, Message::binary()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Send a message

<a name="provide-4"></a>

### provide/4 ###

<pre><code>
provide(Topic::binary() | {binary(), integer()} | {binary(), binary()}, From::binary(), To::binary(), Body::term()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Send a message

<a name="provide-5"></a>

### provide/5 ###

<pre><code>
provide(Topic::binary() | {binary(), integer()} | {binary(), binary()}, From::binary(), To::binary(), Body::term(), Options::list()) -&gt; {ok, term()} | {error, term()}
</code></pre>
<br />

Send a message

<a name="reply-4"></a>

### reply/4 ###

<pre><code>
reply(Msg::<a href="#type-message">message()</a>, Topic::binary() | {binary(), integer()} | {binary(), binary()}, To::binary(), Body::term()) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />


Set the response message

Since the sender is not specified, we will use the recipient of the incoming message

<a name="reply-5"></a>

### reply/5 ###

<pre><code>
reply(Msg::<a href="#type-message">message()</a>, Topic::binary() | {binary(), integer()} | {binary(), binary()}, From::binary(), To::binary(), Body::term()) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Set the response message

<a name="response-1"></a>

### response/1 ###

<pre><code>
response(Message::<a href="#type-message">message()</a>) -&gt; #{}
</code></pre>
<br />

Return the outgoing message as map

<a name="response_body-1"></a>

### response_body/1 ###

<pre><code>
response_body(Wok_message::<a href="#type-message">message()</a>) -&gt; binary()
</code></pre>
<br />

Return the outgoing message body

<a name="response_body-2"></a>

### response_body/2 ###

<pre><code>
response_body(Wok_message::<a href="#type-message">message()</a>, Body::term()) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Update the outgoing message body

<a name="response_from-1"></a>

### response_from/1 ###

<pre><code>
response_from(Wok_message::<a href="#type-message">message()</a>) -&gt; binary()
</code></pre>
<br />

Return the outgoing message from

<a name="response_from-2"></a>

### response_from/2 ###

<pre><code>
response_from(Wok_message::<a href="#type-message">message()</a>, From::binary()) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Update the outgoing message from

<a name="response_to-1"></a>

### response_to/1 ###

<pre><code>
response_to(Wok_message::<a href="#type-message">message()</a>) -&gt; binary()
</code></pre>
<br />

Return the outgoing message to

<a name="response_to-2"></a>

### response_to/2 ###

<pre><code>
response_to(Wok_message::<a href="#type-message">message()</a>, To::binary()) -&gt; <a href="#type-message">message()</a>
</code></pre>
<br />

Update the outgoing message to

<a name="to-1"></a>

### to/1 ###

<pre><code>
to(Wok_message::<a href="#type-message">message()</a>) -&gt; binary() | undefined
</code></pre>
<br />

Return the incoming message to

<a name="uuid-1"></a>

### uuid/1 ###

<pre><code>
uuid(Wok_message::<a href="#type-message">message()</a>) -&gt; binary() | undefined
</code></pre>
<br />

Return the incoming message UUID




# Module wok_message #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

<a name="types"></a>

## Data Types ##




### <a name="type-opaque_message_transfert">opaque_message_transfert()</a> ###


<pre><code>
opaque_message_transfert() = binary()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#async_reply-1">async_reply/1</a></td><td></td></tr><tr><td valign="top"><a href="#body-1">body/1</a></td><td></td></tr><tr><td valign="top"><a href="#content-1">content/1</a></td><td></td></tr><tr><td valign="top"><a href="#content-2">content/2</a></td><td></td></tr><tr><td valign="top"><a href="#content_has_map-1">content_has_map/1</a></td><td></td></tr><tr><td valign="top"><a href="#custom_data-1">custom_data/1</a></td><td></td></tr><tr><td valign="top"><a href="#custom_data-2">custom_data/2</a></td><td></td></tr><tr><td valign="top"><a href="#custom_data-3">custom_data/3</a></td><td></td></tr><tr><td valign="top"><a href="#encode_message-4">encode_message/4</a></td><td></td></tr><tr><td valign="top"><a href="#encode_reply-4">encode_reply/4</a></td><td></td></tr><tr><td valign="top"><a href="#encode_reply-5">encode_reply/5</a></td><td></td></tr><tr><td valign="top"><a href="#from-1">from/1</a></td><td></td></tr><tr><td valign="top"><a href="#global_state-1">global_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#headers-1">headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#local_state-1">local_state/1</a></td><td></td></tr><tr><td valign="top"><a href="#noreply-1">noreply/1</a></td><td></td></tr><tr><td valign="top"><a href="#param-2">param/2</a></td><td></td></tr><tr><td valign="top"><a href="#params-1">params/1</a></td><td></td></tr><tr><td valign="top"><a href="#provide-2">provide/2</a></td><td>
Send a message.</td></tr><tr><td valign="top"><a href="#provide-4">provide/4</a></td><td>
Send a message.</td></tr><tr><td valign="top"><a href="#provide-5">provide/5</a></td><td>
Send a message.</td></tr><tr><td valign="top"><a href="#reply-4">reply/4</a></td><td></td></tr><tr><td valign="top"><a href="#reply-5">reply/5</a></td><td></td></tr><tr><td valign="top"><a href="#response-1">response/1</a></td><td></td></tr><tr><td valign="top"><a href="#response_body-1">response_body/1</a></td><td></td></tr><tr><td valign="top"><a href="#response_body-2">response_body/2</a></td><td></td></tr><tr><td valign="top"><a href="#response_from-1">response_from/1</a></td><td></td></tr><tr><td valign="top"><a href="#response_from-2">response_from/2</a></td><td></td></tr><tr><td valign="top"><a href="#response_to-1">response_to/1</a></td><td></td></tr><tr><td valign="top"><a href="#response_to-2">response_to/2</a></td><td></td></tr><tr><td valign="top"><a href="#to-1">to/1</a></td><td></td></tr><tr><td valign="top"><a href="#uuid-1">uuid/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="async_reply-1"></a>

### async_reply/1 ###

<pre><code>
async_reply(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; <a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>
</code></pre>
<br />

<a name="body-1"></a>

### body/1 ###

<pre><code>
body(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a> | <a href="/home/glejeune/Dropbox/BotsUnit/Dev/msaas/wok/_build/default/lib/wok_message_handler/doc/wok_message_handler.md#type-message">wok_message_handler:message()</a>) -&gt; binary()
</code></pre>
<br />

<a name="content-1"></a>

### content/1 ###

<pre><code>
content(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; <a href="/home/glejeune/Dropbox/BotsUnit/Dev/msaas/wok/_build/default/lib/wok_message_handler/doc/wok_message_handler.md#type-message">wok_message_handler:message()</a>
</code></pre>
<br />

<a name="content-2"></a>

### content/2 ###

<pre><code>
content(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, Message::<a href="/home/glejeune/Dropbox/BotsUnit/Dev/msaas/wok/_build/default/lib/wok_message_handler/doc/wok_message_handler.md#type-message">wok_message_handler:message()</a>) -&gt; <a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>
</code></pre>
<br />

<a name="content_has_map-1"></a>

### content_has_map/1 ###

<pre><code>
content_has_map(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; #{}
</code></pre>
<br />

<a name="custom_data-1"></a>

### custom_data/1 ###

<pre><code>
custom_data(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; #{}
</code></pre>
<br />

<a name="custom_data-2"></a>

### custom_data/2 ###

<pre><code>
custom_data(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, Key::atom()) -&gt; any()
</code></pre>
<br />

<a name="custom_data-3"></a>

### custom_data/3 ###

<pre><code>
custom_data(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, Key::atom(), Value::any()) -&gt; {ok, <a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, any()} | {ok, <a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>}
</code></pre>
<br />

<a name="encode_message-4"></a>

### encode_message/4 ###

<pre><code>
encode_message(Topic::binary() | {Topic::binary(), Partition::integer()} | {Topic::binary(), Key::binary()}, From::binary(), To::binary(), Body::term()) -&gt; {ok, binary(), integer(), <a href="#type-opaque_message_transfert">opaque_message_transfert()</a>} | {error, term()}
</code></pre>
<br />

<a name="encode_reply-4"></a>

### encode_reply/4 ###

<pre><code>
encode_reply(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, Topic::binary() | {Topic::binary(), Partition::integer()} | {Topic::binary(), Key::binary()}, To::binary(), Body::term()) -&gt; {ok, binary(), integer(), <a href="#type-opaque_message_transfert">opaque_message_transfert()</a>} | {error, term()}
</code></pre>
<br />

<a name="encode_reply-5"></a>

### encode_reply/5 ###

<pre><code>
encode_reply(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, Topic::binary() | {Topic::binary(), Partition::integer()} | {Topic::binary(), Key::binary()}, From::binary(), To::binary(), Body::term()) -&gt; {ok, binary(), integer(), <a href="#type-opaque_message_transfert">opaque_message_transfert()</a>} | {error, term()}
</code></pre>
<br />

<a name="from-1"></a>

### from/1 ###

<pre><code>
from(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a> | <a href="/home/glejeune/Dropbox/BotsUnit/Dev/msaas/wok/_build/default/lib/wok_message_handler/doc/wok_message_handler.md#type-message">wok_message_handler:message()</a>) -&gt; binary()
</code></pre>
<br />

<a name="global_state-1"></a>

### global_state/1 ###

<pre><code>
global_state(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; any()
</code></pre>
<br />

<a name="headers-1"></a>

### headers/1 ###

<pre><code>
headers(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a> | <a href="/home/glejeune/Dropbox/BotsUnit/Dev/msaas/wok/_build/default/lib/wok_message_handler/doc/wok_message_handler.md#type-message">wok_message_handler:message()</a>) -&gt; binary()
</code></pre>
<br />

<a name="local_state-1"></a>

### local_state/1 ###

<pre><code>
local_state(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; any()
</code></pre>
<br />

<a name="noreply-1"></a>

### noreply/1 ###

<pre><code>
noreply(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; <a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>
</code></pre>
<br />

<a name="param-2"></a>

### param/2 ###

<pre><code>
param(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a> | <a href="/home/glejeune/Dropbox/BotsUnit/Dev/msaas/wok/_build/default/lib/wok_message_handler/doc/wok_message_handler.md#type-message">wok_message_handler:message()</a>, Param::binary() | list() | atom()) -&gt; binary() | undefined
</code></pre>
<br />

<a name="params-1"></a>

### params/1 ###

<pre><code>
params(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a> | <a href="/home/glejeune/Dropbox/BotsUnit/Dev/msaas/wok/_build/default/lib/wok_message_handler/doc/wok_message_handler.md#type-message">wok_message_handler:message()</a>) -&gt; #{}
</code></pre>
<br />

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
reply(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, Topic::binary() | {binary(), integer()} | {binary(), binary()}, To::binary(), Body::term()) -&gt; <a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>
</code></pre>
<br />

<a name="reply-5"></a>

### reply/5 ###

<pre><code>
reply(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, Topic::binary() | {binary(), integer()} | {binary(), binary()}, From::binary(), To::binary(), Body::term()) -&gt; <a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>
</code></pre>
<br />

<a name="response-1"></a>

### response/1 ###

<pre><code>
response(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; {Topic::term(), From::binary(), To::binary(), Body::term()}
</code></pre>
<br />

<a name="response_body-1"></a>

### response_body/1 ###

<pre><code>
response_body(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; binary()
</code></pre>
<br />

<a name="response_body-2"></a>

### response_body/2 ###

<pre><code>
response_body(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, Body::term()) -&gt; <a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>
</code></pre>
<br />

<a name="response_from-1"></a>

### response_from/1 ###

<pre><code>
response_from(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; binary()
</code></pre>
<br />

<a name="response_from-2"></a>

### response_from/2 ###

<pre><code>
response_from(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, From::binary()) -&gt; <a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>
</code></pre>
<br />

<a name="response_to-1"></a>

### response_to/1 ###

<pre><code>
response_to(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>) -&gt; binary()
</code></pre>
<br />

<a name="response_to-2"></a>

### response_to/2 ###

<pre><code>
response_to(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>, To::binary()) -&gt; <a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a>
</code></pre>
<br />

<a name="to-1"></a>

### to/1 ###

<pre><code>
to(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a> | <a href="/home/glejeune/Dropbox/BotsUnit/Dev/msaas/wok/_build/default/lib/wok_message_handler/doc/wok_message_handler.md#type-message">wok_message_handler:message()</a>) -&gt; binary()
</code></pre>
<br />

<a name="uuid-1"></a>

### uuid/1 ###

<pre><code>
uuid(Msg::<a href="wok_msg.md#type-wok_msg">wok_msg:wok_msg()</a> | <a href="/home/glejeune/Dropbox/BotsUnit/Dev/msaas/wok/_build/default/lib/wok_message_handler/doc/wok_message_handler.md#type-message">wok_message_handler:message()</a>) -&gt; binary()
</code></pre>
<br />




# Module wok_request #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#body-1">body/1</a></td><td></td></tr><tr><td valign="top"><a href="#client_ip-1">client_ip/1</a></td><td></td></tr><tr><td valign="top"><a href="#client_port-1">client_port/1</a></td><td></td></tr><tr><td valign="top"><a href="#cookie-2">cookie/2</a></td><td></td></tr><tr><td valign="top"><a href="#cookies-1">cookies/1</a></td><td></td></tr><tr><td valign="top"><a href="#custom_data-1">custom_data/1</a></td><td>
This function returns wok_req's custom data.</td></tr><tr><td valign="top"><a href="#custom_data-2">custom_data/2</a></td><td>
This function sets wok_req's custom data.</td></tr><tr><td valign="top"><a href="#global_state-1">global_state/1</a></td><td>
This function get global_state of wok req.</td></tr><tr><td valign="top"><a href="#global_state-2">global_state/2</a></td><td>
This function set global_state of wok req.</td></tr><tr><td valign="top"><a href="#header-2">header/2</a></td><td>Equivalent to <a href="#header-3"><tt>header(Req, Name, undefined)</tt></a>.</td></tr><tr><td valign="top"><a href="#header-3">header/3</a></td><td></td></tr><tr><td valign="top"><a href="#headers-1">headers/1</a></td><td></td></tr><tr><td valign="top"><a href="#local_state-1">local_state/1</a></td><td>
This function get local_state of wok req.</td></tr><tr><td valign="top"><a href="#local_state-2">local_state/2</a></td><td>
This function set local_state of wok req.</td></tr><tr><td valign="top"><a href="#method-1">method/1</a></td><td>
This function is an implementation of cowboy_req:method/1 for wok_req.</td></tr><tr><td valign="top"><a href="#param-2">param/2</a></td><td></td></tr><tr><td valign="top"><a href="#param-3">param/3</a></td><td></td></tr><tr><td valign="top"><a href="#param-4">param/4</a></td><td></td></tr><tr><td valign="top"><a href="#params-1">params/1</a></td><td></td></tr><tr><td valign="top"><a href="#params-2">params/2</a></td><td></td></tr><tr><td valign="top"><a href="#path-1">path/1</a></td><td>
This function is an iimplementation of cowboy_req:path/1 for wok_req.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="body-1"></a>

### body/1 ###

`body(Req) -> any()`

<a name="client_ip-1"></a>

### client_ip/1 ###

<pre><code>
client_ip(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>) -&gt; <a href="inet.md#type-ip_address">inet:ip_address()</a>
</code></pre>
<br />

<a name="client_port-1"></a>

### client_port/1 ###

<pre><code>
client_port(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>) -&gt; <a href="inet.md#type-port_number">inet:port_number()</a>
</code></pre>
<br />

<a name="cookie-2"></a>

### cookie/2 ###

<pre><code>
cookie(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>, Name::binary()) -&gt; binary() | undefined
</code></pre>
<br />

<a name="cookies-1"></a>

### cookies/1 ###

<pre><code>
cookies(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>) -&gt; [{binary(), binary()}]
</code></pre>
<br />

<a name="custom_data-1"></a>

### custom_data/1 ###

<pre><code>
custom_data(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>) -&gt; term()
</code></pre>
<br />

This function returns wok_req's custom data

<a name="custom_data-2"></a>

### custom_data/2 ###

<pre><code>
custom_data(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>, Data::term()) -&gt; <a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>
</code></pre>
<br />

This function sets wok_req's custom data

<a name="global_state-1"></a>

### global_state/1 ###

<pre><code>
global_state(WokReq::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>) -&gt; term()
</code></pre>
<br />

This function get global_state of wok req

<a name="global_state-2"></a>

### global_state/2 ###

<pre><code>
global_state(WokReq::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>, GlobalState::term()) -&gt; <a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>
</code></pre>
<br />

This function set global_state of wok req

<a name="header-2"></a>

### header/2 ###

`header(Req, Name) -> any()`

Equivalent to [`header(Req, Name, undefined)`](#header-3).

<a name="header-3"></a>

### header/3 ###

<pre><code>
header(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>, Name::binary(), Default::any()) -&gt; binary() | any() | undefined
</code></pre>
<br />

<a name="headers-1"></a>

### headers/1 ###

<pre><code>
headers(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>) -&gt; [{binary(), iodata()}]
</code></pre>
<br />

<a name="local_state-1"></a>

### local_state/1 ###

<pre><code>
local_state(WokReq::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>) -&gt; term()
</code></pre>
<br />

This function get local_state of wok req

<a name="local_state-2"></a>

### local_state/2 ###

<pre><code>
local_state(WokReq::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>, LocalState::term()) -&gt; <a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>
</code></pre>
<br />

This function set local_state of wok req

<a name="method-1"></a>

### method/1 ###

<pre><code>
method(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>) -&gt; term()
</code></pre>
<br />

This function is an implementation of cowboy_req:method/1 for wok_req

<a name="param-2"></a>

### param/2 ###

<pre><code>
param(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>, Name::term()) -&gt; {ok, binary(), <a href="wok_req.md#type-req">wok_req:req()</a>} | {undefined, <a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>} | {error, <a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>}
</code></pre>
<br />

<a name="param-3"></a>

### param/3 ###

<pre><code>
param(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>, Type::get | post | bind | term(), Name::term()) -&gt; {ok, term(), <a href="wok_req.md#type-req">wok_req:req()</a>} | {undefined, <a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>} | {error, <a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>}
</code></pre>
<br />

<a name="param-4"></a>

### param/4 ###

<pre><code>
param(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>, Type::get | post | bind, Name::term(), Default::term()) -&gt; {ok, term(), <a href="wok_req.md#type-req">wok_req:req()</a>} | {undefined, <a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>} | {error, <a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>}
</code></pre>
<br />

<a name="params-1"></a>

### params/1 ###

<pre><code>
params(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>) -&gt; {ok, list(), <a href="wok_req.md#type-req">wok_req:req()</a>} | {error, <a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>}
</code></pre>
<br />

<a name="params-2"></a>

### params/2 ###

<pre><code>
params(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>, Type::get | post | bind) -&gt; {ok, list(), <a href="wok_req.md#type-req">wok_req:req()</a>} | {error, <a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>}
</code></pre>
<br />

<a name="path-1"></a>

### path/1 ###

<pre><code>
path(Req::<a href="wok_req.md#type-wok_req">wok_req:wok_req()</a>) -&gt; term()
</code></pre>
<br />

This function is an iimplementation of cowboy_req:path/1 for wok_req


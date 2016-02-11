

# Module wok_req #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

This modules responds to all cowboy_req functions.

<a name="types"></a>

## Data Types ##




### <a name="type-wok_req">wok_req()</a> ###


<pre><code>
wok_req() = #wok_req{}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#custom_data-1">custom_data/1</a></td><td>
This function returns wok_req's custom data.</td></tr><tr><td valign="top"><a href="#custom_data-2">custom_data/2</a></td><td>
This function sets wok_req's custom data.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="custom_data-1"></a>

### custom_data/1 ###

<pre><code>
custom_data(Wok_req::<a href="#type-wok_req">wok_req()</a>) -&gt; term()
</code></pre>
<br />

This function returns wok_req's custom data

<a name="custom_data-2"></a>

### custom_data/2 ###

<pre><code>
custom_data(WokReq::<a href="#type-wok_req">wok_req()</a>, CustomData::term()) -&gt; <a href="#type-wok_req">wok_req()</a>
</code></pre>
<br />

This function sets wok_req's custom data


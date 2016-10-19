

# Module wok_message_handler #
* [Data Types](#types)

__This module defines the `wok_message_handler` behaviour.__<br /> Required callback functions: `create/3`, `create/4`, `parse/1`.

<a name="types"></a>

## Data Types ##




### <a name="type-message">message()</a> ###


<pre><code>
message() = #msg{}
</code></pre>




### <a name="type-message_handler_option">message_handler_option()</a> ###


<pre><code>
message_handler_option() = {wok_version, integer()} | {headers, #{}} | {atom(), term()}
</code></pre>


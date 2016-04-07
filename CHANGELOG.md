0.2.2:
  * Add wok_request:file/2 and wok_request:file/3

0.2.1:
  * Add wok_request:file/1

  * Add helper for route with params

  * Update routes loader to avoid overwrite

  * Add route helper

  * Add missing wok_message:custom_data/3

  * [wok_tests 0.0.5]: Support options on response cookies

  * [wok_mail 0.0.2]: Automatically declare Wok.Mailer behaviour when using Wok.Mailer in Elixir

  * [wok_i18n 0.0.2]: Use route helper in admin template

  * [wok_i18n 0.0.2]: Add import/export

  * [wok_i18n 0.0.2]: Add Elixir support (mix.exs)

  * [wok_tmpl_engine 0.0.2]: Bug correction in wok_tmpl_engine:render/4

  * [wok_http_adapter 0.0.3]: Remove old cowboy references

  * Kafe 1.0.2

0.2.0:
  * Custom data is a map and can only be set by key

  * Rewrite template engine. wok_erlang_templates is deprecated, use wok_tmpl_engine

  * Add possibility to start a server on wok start or with a middleware.

  * Ussue #6 : Update wok_response:merge_headers/2

  * wok_erlang_template is deprecated

  * Add wok_tmpl_engine

  * Add wok_i18n

  * wok_http_adapter 0.0.2

  * wok_message_handler 0.1.1

  * wok_producer 0.1.1

  * wok_tmpl_engine 0.0.1

  * wok_i18n 0.0.1

  * wok_tests 0.0.4
    
0.1.1:
  * Assmume that routes/0 in middlewares is mandatory

  * Use wok_message_path in wok_dispatcher

  * CORS corrections

0.0.1:
  * First release

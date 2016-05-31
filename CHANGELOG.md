0.3.0:
  * Fix version for bucs (0.0.1), doteki (0.0.1), lager (3.2.0), tempfile (1.0.1), erlang-uuid (v0.5.0), whisk (0.0.1), gen_smtp (0.10.0)

  * Issue #23 : Allow to start Wok without REST

  * Issues #ç, #10, #18, #19, #20 : Add Kafka 0.9 and 0.10 support

  * Issue #16 : change one_for_one 

  * [wok_producer 0.2.0] : Kafka 0.9/0.10 support

  * [wok_tests 0.1.0] : Add file upload support

  * [wok_http_adapter 0.0.4] : Rewrite HTTP params parser

  * [wok_mail 0.0.3] : Make wok_smtp_mailer:send always return an ok/error tuple

  * [wok_mail 0.0.3] : Handle empty Cc/Bcc lists

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

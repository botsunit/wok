PROJECT = wok

DEP_PLUGINS = mix.mk
BUILD_DEPS = mix.mk
ELIXIR_VERSION = ~> 1.2
ELIXIR_BINDINGS = wok wok_request wok_response

dep_mix.mk = git https://github.com/botsunit/mix.mk.git master

DEPS = lager wok_message wok_message_handler wok_producer pipette kafe cowboy cowboy_default_static_file bucs doteki uuid

dep_lager = git https://github.com/basho/lager.git master
dep_wok_message = git git@gitlab.botsunit.com:msaas/wok_message.git 0.0.2
dep_wok_message_handler = git git@gitlab.botsunit.com:msaas/wok_message_handler.git 0.0.2
dep_wok_producer = git git@gitlab.botsunit.com:msaas/wok_producer.git 0.0.2
dep_pipette = git git@gitlab.botsunit.com:msaas/pipette.git master
dep_kafe = git https://github.com/botsunit/kafe.git master
dep_cowboy_default_static_file = git https://github.com/botsunit/cowboy_default_static_file.git master
dep_cowboy = git https://github.com/ninenines/cowboy.git 2.0.0-pre.3
dep_bucs = git https://github.com/botsunit/bucs.git master
dep_doteki = git https://github.com/botsunit/doteki.git master
dep_uuid = git https://github.com/avtobiff/erlang-uuid.git master

DOC_DEPS = edown

dep_edown = git https://github.com/botsunit/edown.git master

TEST_DEPS = meck

dep_meck = git https://github.com/eproxus/meck.git master

CP = cp
CP_R = cp -r
RM_RF = rm -rf
MKDIR_P = mkdir -p
DATE = $(shell date +"%F %T")

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {edown_target, gitlab} \
						, {top_level_readme, {"./README.md", "https://gitlab.botsunit.com/msaas/${PROJECT}"}}

EUNIT_OPTS = verbose, {report, {eunit_surefire, [{dir, "test"}]}}

include erlang.mk

docs:: edoc images/wok.call.png _doc/doc.yml
	@${MKDIR_P} doc/images
	@${CP} images/*.png doc/images
	@${CP} _doc/* doc

wok.call.png: images/wok.call.gv
	@dot -T png -o images/wok.call.png images/wok.call.gv

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/${PROJECT}.config

release: app mix.all


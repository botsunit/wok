PROJECT = wok

DEPS = lager wok_message wok_message_handler wok_producer pipette kafe cowboy cowboy_default_static_file bucs doteki

dep_lager = git https://github.com/basho/lager.git master
dep_wok_message = git git@gitlab.botsunit.com:msaas/wok_message.git master
dep_wok_message_handler = git git@gitlab.botsunit.com:msaas/wok_message_handler.git master
dep_wok_producer = git git@gitlab.botsunit.com:msaas/wok_producer.git master
dep_pipette = git git@gitlab.botsunit.com:msaas/pipette.git master
dep_kafe = git https://github.com/botsunit/kafe.git master
dep_cowboy_default_static_file = git https://github.com/botsunit/cowboy_default_static_file.git master
dep_cowboy = git https://github.com/ninenines/cowboy.git master
dep_bucs = git https://github.com/botsunit/bucs.git master
dep_doteki = git https://github.com/botsunit/doteki.git master

DOC_DEPS = edown

dep_edown = git https://github.com/botsunit/edown.git master

TEST_DEPS = meck

dep_meck = git https://github.com/eproxus/meck.git master

CP = cp
CP_R = cp -r
RM_RF = rm -rf
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

docs:: wok.call.png
	@${CP} *.png doc

wok.call.png: wok.call.gv
	@dot -T png -o wok.call.png wok.call.gv

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/${PROJECT}.config

rel-dev: deps app
	@${RM_RF} ../${PROJECT}-dev
	git clone git@github.com:botsunit/${PROJECT}.git ../${PROJECT}-dev
	@${CP} rebar.config.release ../${PROJECT}-dev/rebar.config
	@${CP} Makefile.release ../${PROJECT}-dev/Makefile
	@${CP} erlang.mk ../${PROJECT}-dev/erlang.mk
	@${CP} README.md.release ../${PROJECT}-dev/README.md
	@${CP_R} ebin ../${PROJECT}-dev
	@${CP_R} config ../${PROJECT}-dev
	@${CP_R} include ../${PROJECT}-dev
	cd ../${PROJECT}-dev; git add .
	cd ../${PROJECT}-dev; git commit -m "Update ${DATE}"


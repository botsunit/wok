PROJECT = wok

DEPS = lager wok_message wok_message_handler pipette kafe cowboy eutils edown
dep_lager = git https://github.com/basho/lager.git master
dep_wok_message = git git@gitlab.scalezen.com:msaas/wok_message.git erlang-mk
dep_wok_message_handler = git git@gitlab.scalezen.com:msaas/wok_message_handler.git erlang-mk
dep_pipette = git git@gitlab.scalezen.com:msaas/pipette.git erlang-mk
dep_kafe = git https://github.com/homeswap/kafe.git erlang-mk
dep_cowboy = git https://github.com/ninenines/cowboy.git master
dep_eutils = git https://github.com/emedia-project/eutils.git master
dep_edown = git https://github.com/homeswap/edown.git master

CP = cp
CP_R = cp -r
RM_RF = rm -rf

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {edown_target, gitlab} \
						, {top_level_readme, {"./README.md", "https://gitlab.scalezen.com/msaas/wok"}} 

include erlang.mk

docs::
	@${CP} *.png doc

dev: deps app
	@erl -pa ebin include deps/*/ebin deps/*/include -config config/wok.config

rel-dev: deps app
	@${RM_RF} ../wok-dev
	git clone git@github.com:scalezen-developer/wok.git ../wok-dev
	@${CP} rebar.release.config ../wok-dev/rebar.config
	@${CP_R} ebin ../wok-dev
	@${CP_R} config ../wok-dev
	@${CP_R} include ../wok-dev
	cd ../wok-dev; git add . 
	cd ../wok-dev; git commit -m "Update ${DATE}"


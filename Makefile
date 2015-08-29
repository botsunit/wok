REBAR = ./rebar
DOT = dot
GV = $(wildcard *.gv) wok.deps.gv
PNG = $(GV:.gv=.png)
RM = rm
RM_F = rm -f
RM_RF = rm -rf
MKDIR = mkdir -p
CP = cp
CP_R = cp -r
DATE = $(shell date +"%F %T")

.PHONY: compile get-deps test doc

all: compile

compile: get-deps
	@$(REBAR) compile

compile-dev: get-deps-dev
	@$(REBAR) -C rebar.dev.config compile

get-deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

get-deps-dev:
	@$(REBAR) -C rebar.dev.config get-deps
	@$(REBAR) -C rebar.dev.config check-deps

clean:
	@$(REBAR) clean
	$(RM_F) erl_crash.dump

realclean: clean
	@$(REBAR) -C rebar.dev.config delete-deps
	@$(RM_RF) deps
	@$(RM_RF) ebin
	@$(RM_RF) log

test: compile-dev
	@ERL_LIBS="../:deps/*/" $(REBAR) skip_deps=true eunit

doc: compile-dev img
	@$(RM_F) documentation.md
	@$(RM_RF) doc
	@$(REBAR) -C rebar.dev.config doc
	@${CP} *.png doc

dev: compile-dev
	@ERL_LIBS="../:deps/*/" erl -pa ebin include deps/*/ebin deps/*/include -config config/wok.config

img: $(PNG)

%.gv :
	@$(REBAR) graph-deps graph=wok.deps.gv

%.png : %.gv
	@$(DOT) -Tpng -o$@ $<

clean-img:
	@$(RM_F) *.png

rel-dev: realclean compile
	@${RM_RF} ../wok-dev
	git clone git@github.com:scalezen-developer/wok.git ../wok-dev
	@${CP} rebar.release.config ../wok-dev/rebar.config
	@${CP_R} ebin ../wok-dev
	@${CP_R} config ../wok-dev
	@${CP_R} include ../wok-dev
	cd ../wok-dev; git add . 
	cd ../wok-dev; git commit -m "Update ${DATE}"


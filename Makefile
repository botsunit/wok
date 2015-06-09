REBAR = ./rebar
DOT = dot
GV = $(wildcard *.gv) wok.deps.gv
PNG = $(GV:.gv=.png)
RM = rm
RM_F = rm -f
RM_RF = rm -rf

.PHONY: compile get-deps test doc

all: compile doc

compile: get-deps
	@$(REBAR) compile

get-deps:
	@$(REBAR) get-deps
	@$(REBAR) check-deps

clean:
	@$(REBAR) clean
	$(RM_F) erl_crash.dump

realclean: clean
	@$(REBAR) delete-deps
	@$(RM_RF) deps
	@$(RM_RF) ebin
	@$(RM_RF) log

test: compile
	@ERL_LIBS="../:deps/*/" $(REBAR) skip_deps=true eunit

doc:
	@$(RM_F) documentation.md
	@$(RM_RF) doc
	@$(REBAR) doc

dev: compile
	@ERL_LIBS="../:deps/*/" erl -pa ebin include deps/*/ebin deps/*/include -config config/wok.config

img: $(PNG)

%.gv :
	@$(REBAR) graph-deps graph=wok.deps.gv

%.png : %.gv
	@$(DOT) -Tpng -o$@ $<

clean-img:
	@$(RM_F) *.png



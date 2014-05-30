REBAR ?= $(shell which rebar 2> /dev/null || which ./rebar)

BEAMDIR              := ebin
DEPS 		             := deps
DIALYZER_OPTIONS     := -Wrace_conditions --fullpath --no_native

ERLANG_DIALYZER_APPS := erts kernel stdlib
DIALYZER_DEPS        :=
DEPS_PLT             := ./deps.plt
DIALYZER_PLT         := ./local.plt

.PHONY: default
default: compile

.PHONY: all
all: compile test doc typecheck

.PHONY: compile
compile: get-deps
	$(REBAR) compile

.PHONY: clean
clean:
	- rm -rf doc/*.html doc/*.png doc/*.css doc/edoc-info \
		$(DIALYZER_PLT) $(DEPS_PLT)
	$(REBAR) clean

.PHONY: doc
doc:
	$(REBAR) skip_deps=true doc

.PHONY: get-deps
get-deps:
	$(REBAR) get-deps

.PHONY: eunit
eunit: compile
	$(REBAR) skip_deps=true eunit -v

.PHONY: ct
ct: compile
	$(REBAR) skip_deps=true ct

.PHONY: test
test: eunit ct

.PHONY: typecheck
typecheck: compile $(DIALYZER_PLT) $(DEPS_PLT)
	dialyzer $(DIALYZER_OPTIONS) --plt $(DEPS_PLT) -r ebin

$(DEPS_PLT): compile
	dialyzer --plt $(DIALYZER_PLT) --add_to_plt $(DIALYZER_DEPS) \
		--output_plt $(DEPS_PLT)

$(DIALYZER_PLT):
	dialyzer --build_plt --apps $(ERLANG_DIALYZER_APPS) \
		--output_plt $(DIALYZER_PLT)

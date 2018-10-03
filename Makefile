# This is a stub Makefile that invokes GNU make which will read the GNUmakefile
# instead of this file. This provides compatability on systems where GNU make is
# not the system 'make' (eg. most non-linux UNIXes).

REBAR ?= $(shell which rebar 2>/dev/null || which ./rebar)

all: compile

compile:
	$(REBAR) compile

compile_verbose:
	$(REBAR) compile verbose=1

check: test
test: all
	$(REBAR) eunit skip_deps=true

check_verbose: test_verbose
test_verbose: all
	$(REBAR) eunit -v skip_deps=true

docs:
	$(REBAR) doc

deps:
	$(REBAR) get-deps

clean:
	$(REBAR) clean

# SPDX-FileCopyrightText: 2020-2026 Peter Lemenkov <lemenkov@gmail.com>
# SPDX-License-Identifier: Apache-2.0

REBAR ?= $(shell which rebar3 2>/dev/null || echo ./rebar3)

.PHONY: all compile test clean dialyzer xref fmt check-fmt docs

all: compile

compile:
	$(REBAR) compile

test: all
	$(REBAR) eunit

dialyzer:
	$(REBAR) dialyzer

xref:
	$(REBAR) xref

fmt:
	$(REBAR) fmt

check-fmt:
	$(REBAR) fmt --check

docs:
	$(REBAR) edoc

clean:
	$(REBAR) clean
	rm -rf _build

check: test dialyzer xref check-fmt


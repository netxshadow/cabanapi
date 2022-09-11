PROJECT=erl_template

ifeq ($(wildcard rebar3),rebar3)
REBAR3 = $(CURDIR)/rebar3
endif

REBAR3 ?= $(shell test -e `which rebar3` 2>/dev/null && which rebar3 || echo "./rebar3")

ifeq ($(REBAR3),)
REBAR3 = $(CURDIR)/rebar3 
endif

.PHONY: all test clean

all: deps compile release
	reset

run: compile release debug

deps:
	@$(REBAR3) get-deps

compile:
	@$(REBAR3) compile

release:
	@$(REBAR3) release -n ${PROJECT}

tests: eunit ct proper

eunit:
	@$(REBAR3) as testing eunit

ct:
	@$(REBAR3) as testing ct --suite test/ct/test_suite.erl

proper:
	@$(REBAR3) as testing proper

cover: tests
	@$(REBAR3) as testing covertool generate
	@$(REBAR3) as testing cover --verbose

analyze: xref lint dialyzer typer

xref:
	@$(REBAR3) as testing xref

lint:
	@$(REBAR3) as testing lint

dialyzer:
	@$(REBAR3) as testing dialyzer

typer:
	typer -DAPP_NAME=$(PROJECT) -DAPP_MODE=testing --plt $(wildcard .plt/rebar3_*_plt) -I include -r src

clean:
	@$(REBAR3) clean

cleanall:
	rm -rf ./_build && rm -rf ./rebar.lock

production_release:
	@$(REBAR3) as production release -n $(PROJECT)

develop_release:
	@$(REBAR3) as develop release -n $(PROJECT)

test_release:
	@$(REBAR3) as testing release -n $(PROJECT)

debug:
	@$(REBAR3) shell --name ${PROJECT}@192.168.31.9 --setcookie ${PROJECT}


OTP_APPS = kernel stdlib ssl compiler erts crypto public_key hipe inets asn1 mnesia runtime_tools syntax_tools

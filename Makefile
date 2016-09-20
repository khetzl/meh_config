# Makefile for Druid pull and push
.PHONY: clean deps compile dev test

ERL=erl
REBAR=./rebar3

deep-clean: clean
	rm rebar.lock
	rm -rf _build
	rm -rf log

clean:
	$(REBAR) clean

deps:
	$(REBAR) get-deps

compile:
	$(REBAR) compile

dev: compile
	$(ERL) -pa _build/default/lib/*/ebin -config config/sys.config -s meh_config

test:
	$(REBAR) eunit

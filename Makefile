REBAR := tools/rebar
LEIN  := tools/lein

.PHONY: deps

all: hzerl-deps hzerl-compile erl-deps erl-compile

erl-deps:
	$(REBAR) get-deps

erl-compile: 
	$(REBAR) compile	

hzerl-deps:
	$(LEIN) deps

hzerl-compile:
	$(LEIN) uberjar

clean: erl-clean hzerl-clean

erl-clean:
	$(REBAR) clean

hzerl-clean:
	$(LEIN) clean

distclean: clean
	$(REBAR) delete-deps




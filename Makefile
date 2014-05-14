REBAR := tools/rebar
LEIN  := tools/lein

.PHONY: deps

all: hzerl-deps clj erl-deps erl

erl-deps:
	$(REBAR) get-deps

erl: 
	$(REBAR) compile	

hzerl-deps:
	$(LEIN) deps

clj:
	$(LEIN) uberjar

clean: erl-clean hzerl-clean

erl-clean:
	$(REBAR) clean

hzerl-clean:
	$(LEIN) clean

distclean: clean
	$(REBAR) delete-deps




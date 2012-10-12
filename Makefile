REBAR=rebar

all:
	$(REBAR) compile

clean:
	$(REBAR) clean

eunit:
	@$(REBAR) eunit

deps/proper:
	@$(REBAR) -C rebar.tests.config get-deps
	cd deps/proper && $(REBAR) compile

tests: clean deps/proper eunit


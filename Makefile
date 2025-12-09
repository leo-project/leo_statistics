.PHONY: all compile deps test eunit xref dialyzer doc clean distclean

REBAR := rebar3

all: compile xref eunit

compile:
	@$(REBAR) compile

deps:
	@$(REBAR) deps

xref:
	@$(REBAR) xref

eunit:
	@$(REBAR) eunit

test: eunit

dialyzer:
	@$(REBAR) dialyzer

doc:
	@$(REBAR) edoc

clean:
	@$(REBAR) clean

distclean:
	@$(REBAR) clean -a
	@rm -rf _build

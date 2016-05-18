.PHONY: all compile run test clean dialyzer

REBAR=./rebar3

all: compile

compile:
		$(REBAR) compile

run: compile
		erl -pa _build/default/lib/*/ebin -boot start_sasl -config sys.config -s sync -run redis_pool

test:
		$(REBAR) eunit verbose=3
		$(REBAR) ct verbose=3

clean:
		$(REBAR) clean
		rm -rf ./erl_crash.dump

dialyzer:
		$(REBAR) dialyzer
REBAR = ./rebar

all: compile

compile:
	${REBAR} compile

get-deps:
	${REBAR} get-deps

test: get-deps compile
	${REBAR} eunit skip_deps=true

clean:
	${REBAR} clean

run: get-deps compile
	erl -pa deps/*/ebin -pa ./ebin -mnesia dir '"test/Mnesia.test"'

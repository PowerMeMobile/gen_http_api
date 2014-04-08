
all: xref

xref: compile
	@./rebar xref skip_deps=true

compile: get-deps
	@./rebar compile

get-deps:
	@./rebar get-deps

test: xref
	@./rebar eunit skip_deps=true

all: main.exe

deps:
	dune external-lib-deps --missing main.exe

main.exe: deps
	dune build main.exe

test-deps:
	dune external-lib-deps --missing @runtest

test: test-deps
	dune runtest

clean:
	dune clean

.PHONY: deps test-deps test clean

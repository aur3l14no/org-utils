.PHONY: build clean test check journal

build:
	esy dune build

check:
	esy dune build @check

test:
	esy dune runtest

clean:
	rm -rf _build *.install

wip:
	esy dune build @check -w --terminal-persistence=clear-on-rebuild

journal:
	esy dune exec -- journal/migration.exe
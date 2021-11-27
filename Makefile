.PHONY: all test clean distclean build run

all: distclean build test

test: clean

clean:

distclean: clean
	stack clean

build:
	stack build

run:
	stack exec rockto

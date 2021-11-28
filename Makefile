.PHONY: init style all test clean distclean build run

init:
	stack install stylish-haskell

all: style build run

style:
	stylish-haskell -r -i .

test: clean

clean:

distclean: clean
	stack clean

build:
	stack build

run:
	stack exec rockto

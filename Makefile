.PHONY: init style all test clean build run

init:
	stack install stylish-haskell

all: style build run

style:
	stylish-haskell -r -i .

test:
	stack test --test-arguments=--format=progress

clean:
	stack clean

build:
	stack build

run:
	stack exec rockto

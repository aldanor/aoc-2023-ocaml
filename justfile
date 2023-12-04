#!/usr/bin/env just --justfile

set positional-arguments

alias b := build
alias r := run

_default:
    @just --list
deps:
    opam install . --deps-only
build:
    dune build
run *args:
    dune exec aoc -- {{args}}
bench *args:
    dune exec aoc -- -b {{args}}

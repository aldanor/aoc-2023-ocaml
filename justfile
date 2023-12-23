#!/usr/bin/env just --justfile

set positional-arguments

alias b := build
alias r := run
alias rr := run-pr
alias t := dune-test
alias tr := dune-test-pr

_default:
    @just --list
deps:
    opam install . --deps-only
build:
    dune build
run *args:
    dune exec aoc -- {{args}}
run-pr *args:
    dune exec --profile=release aoc -- {{args}}
bench *args:
    dune exec aoc -- -b {{args}}
dune-test *args:
    dune test {{args}}
dune-test-pr *args:
    dune test --profile=release {{args}}

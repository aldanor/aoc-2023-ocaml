# aoc-2023

Advent of Code 2022 -- somewhat optimized OCaml solutions.

Note: many of the days here could be optimized further by using more efficient data structures
or more intricate algorithms. However, the point of this run, first and foremost, was for me to 
get familiar with OCaml (which I had zero experience with until day 1). In the end, that goal
was completed: I've tried out various types from Core, built some lexers/parsers with menhir,
built some character-level imperative parsers manually, tested various containers from Base/Core
like hashtables, heaps and queues, checked out bigints and bignums, and lots of other stuff,
like ppx, sexps, formatting, opam/dune tooling, benching tools, and much more). Also, some of 
the solutions will run faster with Flambda, but, again, that's not the main point here. That
being said... I believe most solutions here should be 'faster than average' (as long as you
don't compare them with Rust/C++-like languages).

These are the benchmark results taken on Apple M1; for some problems, there's a shared parsing
step that's used in both parts and is benched separately.

```
  day       part 1    part 2    parse
------------------------------------------
  day 01    70.0 μs    127 μs
  day 02     7.8 μs   10.1 μs    136 μs
  day 03    89.1 μs   64.7 μs
  day 04    39.4 μs   40.9 μs    0.8 μs
  day 05     2.1 μs   31.9 μs   29.0 μs
  day 06    0.17 μs   0.16 μs   0.21 μs
  day 07     158 μs    169 μs
  day 08    83.3 μs    121 μs   1.86 MS
  day 09    45.1 μs   43.5 μs   88.0 μs
  day 10     149 μs    318 μs
  day 11    0.40 μs   0.40 μs   17.4 μs
  day 12     635 μs   6.90 MS
  day 13     4.9 μs    4.6 μs   24.0 μs
  day 14    28.8 μs   52.3 MS
  day 15    92.7 μs    134 μs
  day 16     119 μs   11.1 MS
  day 17    43.0 MS    118 MS
  day 18    14.2 μs   26.6 μs
  day 19     187 μs    191 μs
  day 20     985 μs   3.90 MS   47.1 μs
  day 21     292 μs   4.76 MS
  day 22     182 μs    403 μs
  day 23     593 μs   13.6 MS
  day 24    12.8 MS   16.4 μs    92.9μs
  day 25    1.16 MS             1.15 MS
------------------------------------------
```

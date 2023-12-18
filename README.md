# aoc-2023

Advent of Code 2022 -- somewhat optimized OCaml solutions.

These are the benchmark results taken on Apple M1; for some problems, there's a shared parsing
step that's used in both parts.

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
------------------------------------------
```

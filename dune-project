(lang dune 3.0)
(using menhir 2.1)
(name aoc)
(generate_opam_files true)
(package
  (name aoc)
  (synopsis "Advent of Code 2023 (OCaml)")
  (license "MIT")
  (maintainers "Ivan Smirnov <github@ivan.smirnov.ie>")
  (authors "Ivan Smirnov <github@ivan.smirnov.ie>")
  (homepage "https://github.com/aldanor/aoc-2023-ocaml")
  (bug_reports "https://github.com/aldanor/aoc-2023-ocaml/issues")
  (depends
    base
    core
    core_bench
    core_unix
    stdio
    mtime
    menhir
    fmt
    cohttp-lwt-unix
    lwt_ssl
    ppx_expect
    ppx_sexp_conv
    ppx_jane
    ppx_deriving
    pp-binary-ints
    ocaml-stdint
  )
)

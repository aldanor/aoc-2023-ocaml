(lang dune 2.7)
(using menhir 2.1)
(name aoc)
(generate_opam_files true)
(package
  (name aoc)
  (depends
    base
    core
    core_unix
    stdio
    mtime
    menhir
    fmt
    cohttp-lwt-unix
    lwt_ssl
    ppx_expect
  )
)

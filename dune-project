(lang dune 2.7)
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
    cohttp-lwt-unix
    lwt_ssl
    ppx_expect
  )
)

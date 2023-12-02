open! Base

let print_endline_int i = Stdlib.print_endline (Int.to_string i)

let time f =
  let before = Unix.gettimeofday () in
  let result = f () in
  let after = Unix.gettimeofday () in
  Stdlib.print_endline (Printf.sprintf "%f" (after -. before)) ;
  result

let fmt_elapsed_nice ns_total =
  let ns = ns_total % 1_000_000_000 in
  let s = ns_total / 1_000_000_000 in
  let ms_f = Float.of_int ns /. 1_000_000. in
  let fmt = Printf.sprintf "%02ds %.3fms" in
  fmt s ms_f

let time_m f =
  let before = Mtime_clock.elapsed () in
  let result = f () in
  let after = Mtime_clock.elapsed () in
  let elapsed_ns =
    Mtime.Span.abs_diff before after
    |> Mtime.Span.to_uint64_ns |> Int64.to_int_exn
  in
  Stdlib.print_endline (fmt_elapsed_nice elapsed_ns) ;
  result

let parse_digit c =
  match c with
  | '0' .. '9' -> Some (Char.to_int c - Char.to_int '0')
  | _ -> None

let sum_ints l = List.fold l ~init:0 ~f:( + )

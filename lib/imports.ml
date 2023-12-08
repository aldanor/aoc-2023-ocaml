open Base

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

let parse_digit_unchecked c = Char.to_int c - Char.to_int '0'

let sum_ints l = List.fold l ~init:0 ~f:( + )

let str_find_char_exn s c =
  s |> String.findi ~f:(fun _ x -> Char.(x = c)) |> Option.value_exn |> fst

module StreamParser = struct
  type t = {s: string; n: int; mutable pos: int}

  let create s = {s; n= String.length s; pos= 0}

  let parse_int ?(skip = 1) p =
    let rec parse_int' acc =
      if p.pos = p.n then acc
      else
        let c = String.get p.s p.pos in
        match c with
        | '0' .. '9' as c ->
            p.pos <- p.pos + 1 ;
            parse_int' ((acc * 10) + Char.to_int c - 48)
        | _ -> acc
    in
    let ans = parse_int' 0 in
    p.pos <- p.pos + skip ;
    ans

  let parse_int2 ?skip p =
    let a = parse_int ?skip p in
    let b = parse_int ?skip p in
    (a, b)

  let parse_int3 ?skip p =
    let a = parse_int ?skip p in
    let b = parse_int ?skip p in
    let c = parse_int ?skip p in
    (a, b, c)

  let parse_digit_u p =
    let d = String.unsafe_get p.s p.pos |> parse_digit_unchecked in
    p.pos <- p.pos + 1 ;
    d

  let parse_char_u p =
    let d = String.unsafe_get p.s p.pos in
    p.pos <- p.pos + 1 ;
    d

  let get_u ?(pos = 0) p = String.unsafe_get p.s (p.pos + pos)

  let pos p = p.pos

  let skip p n = p.pos <- p.pos + n

  let is_eof p = p.pos >= p.n

  let not_eof p = p.pos < p.n

  let is_whitespace_u p =
    let c = String.unsafe_get p.s p.pos in
    Char.(c = ' ' || c = '\n')

  let is_newline_u p =
    let c = String.unsafe_get p.s p.pos in
    Char.(c = '\n')

  let not_whitespace p = not_eof p && not (is_whitespace_u p)

  let hd_u p = String.unsafe_get p.s p.pos

  let hd_equals_u p c = Char.(String.unsafe_get p.s p.pos = c)

  let skip_to p c =
    while (not (is_eof p)) && not (hd_equals_u p c) do
      skip p 1
    done

  let skip_whitespace p =
    while (not (is_eof p)) && is_whitespace_u p do
      skip p 1
    done

  let skip_while p ~f =
    while (not (is_eof p)) && hd_u p |> f do
      skip p 1
    done

  let skip_until p ~f =
    while (not (is_eof p)) && not (hd_u p |> f) do
      skip p 1
    done

  let is_hd p ~f = (not (is_eof p)) && hd_u p |> f

  let sub_u ?(pos = 0) p ~len = String.unsafe_sub p.s ~pos:(pos + p.pos) ~len

  let parse_sub_u p ~len =
    let s = String.unsafe_sub p.s ~pos:p.pos ~len in
    p.pos <- p.pos + len ;
    s
end

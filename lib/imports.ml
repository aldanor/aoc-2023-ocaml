open! Base

let print_endline_int i = Stdlib.print_endline (Int.to_string i)

let time f =
  let before = Unix.gettimeofday () in
  let result = f () in
  let after = Unix.gettimeofday () in
  Stdlib.print_endline (Printf.sprintf "%f" (after -. before)) ;
  result

let parse_digit c =
  match c with
  | '0' .. '9' -> Some (Char.to_int c - Char.to_int '0')
  | _ -> None

open Imports
open Base
open Core

module Game = struct
  type t =
    { s: string
    ; n_win: int
    ; n_num: int
    ; n_lines: int
    ; n_pre: int
    ; line_len: int
    ; arr: int array }

  let create s =
    let n_pre = str_find_char_exn s ':' + 2 in
    let n_win = (str_find_char_exn s '|' - n_pre) / 3 in
    let n_num = ((str_find_char_exn s '\n' - n_pre - 1) / 3) - n_win in
    let line_len = 1 + (1 + n_pre + (3 * (n_win + n_num))) in
    let n_lines = (1 + String.length s) / line_len in
    let arr = Array.create ~len:100 (-1) in
    {s; n_win; n_num; n_lines; n_pre; line_len; arr}

  let n_win g line_id =
    let digit i = String.unsafe_get g.s i |> parse_digit_unchecked in
    let parse_num2 i = (10 * (digit i |> Int.max 0)) + digit (i + 1) in
    let i = ref ((line_id * g.line_len) + g.n_pre) in
    for _ = 0 to g.n_win - 1 do
      let k = parse_num2 !i in
      Array.unsafe_set g.arr k line_id ;
      i := !i + 3
    done ;
    let n_win = ref 0 in
    let i = ref ((line_id * g.line_len) + g.n_pre + 2 + (3 * g.n_win)) in
    for _ = 0 to g.n_num - 1 do
      let k = parse_num2 !i in
      let v = Array.unsafe_get g.arr k in
      if v = line_id then n_win := !n_win + 1 ;
      i := !i + 3
    done ;
    !n_win
end

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 15268 *)
    let g = Game.create s in
    let ans = ref 0 in
    for line_id = 0 to g.n_lines - 1 do
      let n_win = Game.n_win g line_id in
      if not (n_win = 0) then ans := !ans + Int.shift_left 1 (n_win - 1)
    done ;
    !ans |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\n\
   Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\n\
   Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\n\
   Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\n\
   Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\n\
   Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11" |> run_test ;
  [%expect {| 13 30 |}]

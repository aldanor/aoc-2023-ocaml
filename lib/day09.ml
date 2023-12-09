open Core
open Imports

let rec reduce arr k v =
  let n, all_zero = (Array.length arr, ref true) in
  for i = n - 1 downto k do
    let diff = Array.unsafe_get arr (i - 1) - Array.unsafe_get arr i in
    Array.unsafe_set arr i diff ;
    all_zero := !all_zero && diff = 0
  done ;
  let v = v + Array.unsafe_get arr (k - 1) in
  if !all_zero then v else reduce arr (k + 1) v

let solve line_to_array lines =
  let extrapolate line = reduce (line_to_array line) 1 0 in
  lines |> List.fold ~init:0 ~f:(fun acc line -> acc + extrapolate line)

module M = struct
  type t = int list list

  let parse s =
    let module P = StreamParser in
    let p = P.create s in
    let lines = ref [] in
    while P.not_eof p do
      let line = ref [P.parse_signed_int ~skip:0 p] in
      while P.not_newline p do
        P.skip p 1 ;
        let num = P.parse_signed_int ~skip:0 p in
        line := num :: !line
      done ;
      P.skip p 1 ;
      lines := !line :: !lines
    done ;
    !lines

  let part1 lines =
    (* 1921197370 *)
    lines |> solve Array.of_list |> Int.to_string

  let part2 lines =
    (* 1124 *)
    lines |> solve Array.of_list_rev |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45" |> run_test ;
  [%expect {| 114 2 |}]

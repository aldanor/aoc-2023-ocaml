open Core
open Imports

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
    let extrapolate_line line =
      let arr = Array.of_list line in
      let n = Array.length arr in
      let rec reduce k =
        let all_zero = ref true in
        for i = n - 1 downto k do
          let diff = arr.(i - 1) - arr.(i) in
          arr.(i) <- diff ;
          all_zero := !all_zero && diff = 0
        done ;
        if !all_zero then k else reduce (k + 1)
      in
      let k = reduce 1 in
      let sum = ref 0 in
      for i = 0 to k - 1 do
        sum := !sum + arr.(i)
      done ;
      !sum
    in
    lines
    |> List.fold ~init:0 ~f:(fun acc line -> acc + extrapolate_line line)
    |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45" |> run_test ;
  [%expect {| 114 |}]

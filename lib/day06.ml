open Imports
open Core

module M = struct
  type t = (int * int) list

  let parse s =
    let module P = StreamParser in
    let parse_line p =
      P.skip_until p ~f:Char.is_digit ;
      let numbers = ref [] in
      while P.is_hd p ~f:Char.is_digit do
        let num = P.parse_int p in
        numbers := num :: !numbers ;
        P.skip_while p ~f:(fun c -> Char.(c = ' '))
      done ;
      List.rev !numbers
    in
    let p = P.create s in
    let time = parse_line p in
    let distance = parse_line p in
    List.zip_exn time distance

  let part1 races =
    (* 281600 *)
    let compute_distance time t = t * (time - t) in
    let count_options (time, distance) =
      List.range 0 (time + 1)
      |> List.count ~f:(fun t -> compute_distance time t > distance)
    in
    List.map races ~f:count_options
    |> List.fold ~init:1 ~f:( * )
    |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "Time:      7  15   30\nDistance:  9  40  200" |> run_test ;
  [%expect {| 288 |}]

open Imports
open Core

let count_win_options time distance =
  let open Float in
  let time, distance = (of_int time, of_int distance) in
  let sq_d = sqrt ((time ** 2.) - (4. * distance)) in
  let l, r = ((time - sq_d) / 2., (time + sq_d) / 2.) in
  round_up r - round_down l - 1. |> to_int

module M = struct
  type t = int list * int list

  let parse s =
    let module P = StreamParser in
    let parse_line p =
      let open Char in
      P.skip_until p ~f:is_digit ;
      let numbers = ref [] in
      while P.is_hd p ~f:is_digit do
        numbers := P.parse_int p :: !numbers ;
        P.skip_while p ~f:(equal ' ')
      done ;
      List.rev !numbers
    in
    let p = P.create s in
    let time = parse_line p in
    let distance = parse_line p in
    (time, distance)

  let part1 (time, distance) =
    (* 281600 *)
    let count_options (time, distance) = count_win_options time distance in
    List.(
      zip_exn time distance |> map ~f:count_options |> fold ~init:1 ~f:( * ) )
    |> Int.to_string

  let part2 (time, distance) =
    (* 33875953 *)
    let rec n_digits' p acc n =
      if n < p then acc else n_digits' (p * 10) (acc + 1) n
    in
    let n_digits = n_digits' 10 1 in
    let concat =
      List.fold ~init:0 ~f:(fun acc n -> (acc * Int.(10 ** n_digits n)) + n)
    in
    let time, distance = (time |> concat, distance |> concat) in
    count_win_options time distance |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "Time:      7  15   30\nDistance:  9  40  200" |> run_test ;
  [%expect {| 288 71503 |}]

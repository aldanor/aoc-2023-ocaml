open Base
open Lexing
open Imports

let err_msg lexbuf msg =
  let pos = lexbuf.lex_curr_p in
  let pos_str =
    Fmt.str "%d:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)
  in
  match msg with
  | None -> Fmt.str "%s: syntax error" pos_str
  | Some msg -> Fmt.str "%s: %s" pos_str msg

exception Error of string

module M = struct
  (* list of games; each game has rounds; round = (color, count) pairs *)
  type t = (int * int) list list list

  let parse inputs =
    let lexbuf = Lexing.from_string inputs in
    try Day02p.prog Day02l.read lexbuf with
    | Day02l.SyntaxError msg -> raise (Error (err_msg lexbuf (Some msg)))
    | Day02p.Error -> raise (Error (err_msg lexbuf None))
    | Failure msg -> raise (Error (err_msg lexbuf (Some msg)))

  let part1 games =
    (* 2237 *)
    let max = [|12; 13; 14|] in
    let count_ok (color, n) = n <= max.(color) in
    let game_ok game = game |> List.concat |> List.for_all ~f:count_ok in
    let game_score i game = if game_ok game then i + 1 else 0 in
    games |> List.mapi ~f:game_score |> sum_ints |> Int.to_string

  let part2 games =
    (* 66681 *)
    let game_power game =
      let max = [|1; 1; 1|] in
      let update_max (color, n) = max.(color) <- Int.max max.(color) n in
      game |> List.concat |> List.iter ~f:update_max ;
      Array.fold max ~init:1 ~f:( * )
    in
    games |> List.map ~f:game_power |> sum_ints |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\n\
   Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\n\
   Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\n\
   Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\n\
   Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green" |> run_test ;
  [%expect {| 8 2286 |}]

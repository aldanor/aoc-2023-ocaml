open! Imports
open Core
open Lexing
open Base

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
    let count_ok (color, count) = count <= max.(color) in
    let game_ok game = game |> List.concat |> List.for_all ~f:count_ok in
    let fold_game i acc game = if game_ok game then acc + i + 1 else acc in
    let ans = List.foldi games ~init:0 ~f:fold_game in
    Printf.sprintf "%d" ans |> Stdlib.print_endline

  let part2 games =
    (* 66681 *)
    let game_power game =
      let max = [|1; 1; 1|] in
      let update_max (color, count) =
        max.(color) <- Int.max max.(color) count
      in
      let () = game |> List.concat |> List.iter ~f:update_max in
      Array.fold max ~init:1 ~f:( * )
    in
    games |> List.map ~f:game_power
    |> List.fold ~init:0 ~f:( + )
    |> Printf.sprintf "%d" |> Stdlib.print_endline
end

include M
include Day.Make (M)

let example = ""

let%expect_test _ = run example ; [%expect {| |}]

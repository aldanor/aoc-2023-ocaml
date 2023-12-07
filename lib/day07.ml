open Core
open Imports

(** parse card rank, from AKQJT9..2 to 12..0 *)
let parse_card_rank = function
  | 'A' -> 12
  | 'K' -> 11
  | 'Q' -> 10
  | 'J' -> 9
  | 'T' -> 8
  | c -> Char.to_int c - Char.to_int '0' - 2

(* TODO: remove *)
let card_to_char card =
  match card with
  | 12 -> 'A'
  | 11 -> 'K'
  | 10 -> 'Q'
  | 9 -> 'J'
  | 8 -> 'T'
  | c -> Char.of_int_exn (c + Char.to_int '0' + 2)

(* TODO: remove *)
let hand_to_string hand =
  hand |> Array.map ~f:card_to_char |> String.of_array

(* TODO: remove *)
let hand_type_to_string = function
  | 1 -> "one pair"
  | 2 -> "two pair"
  | 3 -> "three of a kind"
  | 4 -> "full house"
  | 5 -> "four of a kind"
  | 6 -> "five of a kind"
  | _ -> "high card"

(** one of the optimal 5-element sorting networks *)
let sort5 arr =
  let compare_and_swap i j arr =
    if arr.(i) > arr.(j) then (
      let tmp = arr.(i) in
      arr.(i) <- arr.(j) ;
      arr.(j) <- tmp )
  in
  compare_and_swap 0 1 arr ;
  compare_and_swap 2 3 arr ;
  compare_and_swap 0 2 arr ;
  compare_and_swap 1 4 arr ;
  compare_and_swap 0 1 arr ;
  compare_and_swap 2 3 arr ;
  compare_and_swap 1 2 arr ;
  compare_and_swap 3 4 arr ;
  compare_and_swap 2 3 arr ;
  ()

(** evaluate the hand rank *)
let hand_rank hand =
  let sorted = Array.copy hand in
  sort5 sorted ;
  let s = Array.unsafe_get sorted in
  let diff = (s 1 - s 0, s 2 - s 1, s 3 - s 2, s 4 - s 3) in
  let hand_type =
    match diff with
    (* five of a kind *)
    | 0, 0, 0, 0 -> 6
    (* four of a kind *)
    | 0, 0, 0, _ | _, 0, 0, 0 -> 5
    (* full house *)
    | 0, 0, _, 0 | 0, _, 0, 0 -> 4
    (* three of a kind *)
    | 0, 0, _, _ | _, 0, 0, _ | _, _, 0, 0 -> 3
    (* two pair *)
    | 0, _, 0, _ | _, 0, _, 0 | 0, _, _, 0 -> 2
    (* one pair *)
    | 0, _, _, _ | _, 0, _, _ | _, _, 0, _ | _, _, _, 0 -> 1
    (* high card *)
    | _ -> 0
  in
  let h = Array.unsafe_get hand in
  (* these are powers of 13 which ocaml doesn't want to inline somewhy *)
  (hand_type * 371_293)
  + (h 0 * 28_561)
  + (h 1 * 2_197)
  + (h 2 * 169)
  + (h 3 * 13)
  + h 4

let parse_input s =
  let module P = StreamParser in
  let p = P.create s in
  let hands = ref [] in
  while P.not_eof p do
    (* got to unroll the loop manually, ocaml doesn't want to do it for us *)
    let c0 = P.get_u p ~pos:0 |> parse_card_rank in
    let c1 = P.get_u p ~pos:1 |> parse_card_rank in
    let c2 = P.get_u p ~pos:2 |> parse_card_rank in
    let c3 = P.get_u p ~pos:3 |> parse_card_rank in
    let c4 = P.get_u p ~pos:4 |> parse_card_rank in
    let hand = [|c0; c1; c2; c3; c4|] in
    P.skip p 6 ;
    let score = P.parse_int p in
    (* printf "hand: %s, score: %d\n" (hand_to_string hand) score ; *)
    let hand_rank = hand_rank hand in
    hands := (hand_rank, score) :: !hands
  done ;
  !hands

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 246424613 *)
    parse_input s
    |> List.sort ~compare:(fun (a, _) (b, _) -> a - b)
    |> List.foldi ~init:0 ~f:(fun i acc (_, score) ->
           ((i + 1) * score) + acc )
    |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483" |> run_test ;
  [%expect {| 6440 |}]

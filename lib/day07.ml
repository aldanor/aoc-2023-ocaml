open Core
open Imports

let _J = 14

(** parse card rank, from AKQJT9..2 to 17..5 *)
let parse_card_rank = function
  (* reserve 0..=4 for jokers, so '2' maps to 5 *)
  | 'A' -> 17
  | 'K' -> 16
  | 'Q' -> 15
  | 'J' -> _J
  | 'T' -> 13
  | c -> Char.to_int c - Char.to_int '0' + 3

(** one of the optimal 5-element sorting networks *)
let sort5 arr =
  let cas i j arr =
    let ai, aj = (Array.unsafe_get arr i, Array.unsafe_get arr j) in
    if ai > aj then (Array.unsafe_set arr i aj ; Array.unsafe_set arr j ai) ;
    arr
  in
  arr |> cas 0 1 |> cas 2 3 |> cas 0 2 |> cas 1 4 |> cas 0 1 |> cas 2 3
  |> cas 1 2 |> cas 3 4 |> cas 2 3 |> ignore

let _FIVE, _FOUR, _FULL_HOUSE, _THREE, _TWO, _ONE, _HIGH =
  (6, 5, 4, 3, 2, 1, 0)

(** given an unsorted hand of 5 cards, evaluate its rank, 0..=6 *)
let evaluate_hand_type hand =
  let sorted = Array.copy hand in
  sort5 sorted ;
  let s = Array.unsafe_get sorted in
  match (s 1 - s 0, s 2 - s 1, s 3 - s 2, s 4 - s 3) with
  | 0, 0, 0, 0 -> _FIVE
  | 0, 0, 0, _ | _, 0, 0, 0 -> _FOUR
  | 0, 0, _, 0 | 0, _, 0, 0 -> _FULL_HOUSE
  | 0, 0, _, _ | _, 0, 0, _ | _, _, 0, 0 -> _THREE
  | 0, _, 0, _ | _, 0, _, 0 | 0, _, _, 0 -> _TWO
  | 0, _, _, _ | _, 0, _, _ | _, _, 0, _ | _, _, _, 0 -> _ONE
  | _ -> _HIGH

(** given the hand and its type, zip them into a single number for sorting *)
let get_numeric_hand_rank hand hand_type =
  let h = Array.unsafe_get hand in
  (* these are powers of 18 which ocaml doesn't want to inline somewhy *)
  (hand_type * 1_889_568)
  + (h 0 * 104_976)
  + (h 1 * 5_832)
  + (h 2 * 324)
  + (h 3 * 18)
  + h 4

(** given a hand type with no jokers + number of jokers, update the hand type *)
let update_hand_type_with_jokers n_jokers hand_type =
  (* we assume that jokers don't participate in initial hand type eval *)
  match (hand_type, n_jokers) with
  | _, 0 -> hand_type (* no jokers *)
  | 5, _ -> _FIVE (* four of a kind -> five of a kind *)
  | 3, 2 -> _FIVE (* three of a kind + 2J -> five of a kind *)
  | 3, 1 -> _FOUR (* three of a kind + 1J -> four of a kind *)
  | 2, _ -> _FULL_HOUSE (* two pair -> full house *)
  | 1, 3 -> _FIVE (* one pair + 3J -> five of a kind *)
  | 1, 2 -> _FOUR (* one pair + 2J -> four of a kind *)
  | 1, 1 -> _THREE (* one pair + 1J -> three of a kind *)
  | 0, 4 | 0, 5 -> _FIVE (* high card + 4J -> five of a kind *)
  | 0, 3 -> _FOUR (* high card + 3J -> four of a kind *)
  | 0, 2 -> _THREE (* high card + 2J -> three of a kind *)
  | 0, 1 -> _ONE (* high card + 1J -> one pair *)
  | _ -> hand_type (* shouldn't happen *)

(** evaluate the hand rank with no jokers *)
let hand_rank_default hand =
  hand |> evaluate_hand_type |> get_numeric_hand_rank hand

(** evaluate the hand rank with jokers *)
let hand_rank_jokers hand =
  let n_jokers = ref 0 in
  for i = 0 to 4 do
    if Array.unsafe_get hand i = _J then (
      Array.unsafe_set hand i i ; incr n_jokers )
  done ;
  hand |> evaluate_hand_type
  |> update_hand_type_with_jokers !n_jokers
  |> get_numeric_hand_rank hand

let parse_and_eval_ranks ~hand_rank s =
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
    hands := (hand_rank hand, score) :: !hands
  done ;
  !hands

let parse_and_fold_score ~hand_rank s =
  parse_and_eval_ranks ~hand_rank s
  |> List.sort ~compare:(fun (a, _) (b, _) -> a - b)
  |> List.foldi ~init:0 ~f:(fun i acc (_, score) -> ((i + 1) * score) + acc)

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 246424613 *)
    s |> parse_and_fold_score ~hand_rank:hand_rank_default |> Int.to_string

  let part2 s =
    (* 248256639 *)
    s |> parse_and_fold_score ~hand_rank:hand_rank_jokers |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483" |> run_test ;
  [%expect {| 6440 5905 |}]

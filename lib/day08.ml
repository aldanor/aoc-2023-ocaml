open Core

(* note: reverse order, so that last char is most significant *)
(* note: 2x to indicate left or right branch *)
let pack_int i1 i2 i3 = ((i3 * 26 * 26) + (i2 * 26) + i1) * 2

let _A = Char.to_int 'A'

let char_offset = pack_int _A _A _A

let pack_char c1 c2 c3 =
  pack_int (Char.to_int c1) (Char.to_int c2) (Char.to_int c3) - char_offset

let unpack_to_string idx =
  (* for debugging purposes only *)
  let idx = idx / 2 in
  let divmod x = (x / 26, x mod 26) in
  let idx, i1 = divmod idx in
  let i3, i2 = divmod idx in
  List.map ~f:(fun c -> Char.of_int_exn (c + _A)) [i1; i2; i3]
  |> String.of_char_list

module State = struct
  type t =
    { dirs: int array
    ; arr: int array
    ; mutable pos: int
    ; mutable step: int
    ; mutable count: int }

  let create ?(step = 0) dirs arr pos = {dirs; arr; pos; step; count= 0}

  let reset ?(step = 0) state pos =
    state.pos <- pos ;
    state.step <- step ;
    state.count <- 0

  let next state =
    let d = Array.unsafe_get state.dirs state.step in
    state.step <-
      ( if state.step <> Array.length state.dirs - 1 then state.step + 1
        else 0 ) ;
    let index = state.pos lor d in
    state.pos <- Array.unsafe_get state.arr index ;
    state.count <- state.count + 1

  let to_string {pos; step; count; _} =
    sprintf "pos: %s, step: %d (count: %d)" (unpack_to_string pos) step count

  let equals {pos= pos1; step= step1; _} {pos= pos2; step= step2; _} =
    pos1 = pos2 && step1 = step2

  let brent ?(step = 0) dirs arr pos =
    (* returns: (state, lambda) *)
    (* state: the first state that appear in the cycle *)
    (* lambda: length of the cycle *)
    let power, lambda = (ref 1, ref 1) in
    let tortoise = create dirs arr pos ~step in
    let hare = create dirs arr pos ~step in
    next hare ;
    while not (equals tortoise hare) do
      if !power = !lambda then (
        tortoise.pos <- hare.pos ;
        tortoise.step <- hare.step ;
        power := !power * 2 ;
        lambda := 0 ) ;
      next hare ;
      incr lambda
    done ;
    reset tortoise pos ~step ;
    reset hare pos ~step ;
    for _ = 0 to !lambda - 1 do
      next hare
    done ;
    while not (equals tortoise hare) do
      next tortoise ; next hare
    done ;
    (tortoise, !lambda)
end

module M = struct
  type t = {dirs: int array; arr: int array; a: int list; z: int list}

  let parse s =
    let module P = Imports.StreamParser in
    let p = P.create s in
    let dirs = ref [] in
    while not (P.is_newline_u p) do
      let dir = match P.parse_char_u p with 'L' -> 0 | _ -> 1 in
      dirs := dir :: !dirs
    done ;
    P.skip p 2 ;
    let parse_dest i =
      let c1 = P.get_u p ~pos:i in
      let c2 = P.get_u p ~pos:(i + 1) in
      let c3 = P.get_u p ~pos:(i + 2) in
      (pack_char c1 c2 c3, c3)
    in
    let arr = Array.create ~len:(Int.(26 ** 3) * 2) (-1) in
    let a, z = (ref [], ref []) in
    while P.not_eof p do
      let src, c3 = parse_dest 0 in
      if Char.(c3 = 'A') then a := src :: !a
      else if Char.(c3 = 'Z') then z := src :: !z ;
      let left, _ = parse_dest 7 in
      let right, _ = parse_dest 12 in
      Array.unsafe_set arr src left ;
      Array.unsafe_set arr (src + 1) right ;
      P.skip p 17
    done ;
    let dirs = Array.of_list_rev !dirs in
    {dirs; arr; a= !a; z= !z}

  let part1 {dirs; arr; _} =
    (* 22411 *)
    let start, finish = (pack_char 'A' 'A' 'A', pack_char 'Z' 'Z' 'Z') in
    let state = State.create dirs arr start in
    while state.pos <> finish do
      State.next state
    done ;
    state.count |> Int.to_string

  let part2 {dirs; arr; a; _} =
    (* 11188774513823 *)
    let cycles =
      List.map a ~f:(fun start ->
          let state, len = State.brent dirs arr start in
          let z_min, z_nodes = (pack_char 'A' 'A' 'Z', ref []) in
          for _ = 1 to len do
            if state.pos >= z_min then z_nodes := state.count :: !z_nodes ;
            State.next state
          done ;
          match !z_nodes with
          | [z] when z % len = 0 -> len
          | _ -> failwith "unexpected" )
    in
    let rec gcd a = function 0 -> a | b -> gcd b (a mod b) in
    let lcm a b = a * b / gcd a b in
    List.fold ~init:1 ~f:lcm cycles |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "RL\n\n\
   AAA = (BBB, CCC)\n\
   BBB = (DDD, EEE)\n\
   CCC = (ZZZ, GGG)\n\
   DDD = (DDD, DDD)\n\
   EEE = (EEE, EEE)\n\
   GGG = (GGG, GGG)\n\
   ZZZ = (ZZZ, ZZZ)" |> run_test ~part:1 ;
  [%expect {| 2 |}]

let%expect_test _ =
  "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)"
  |> run_test ~part:1 ;
  [%expect {| 6 |}]

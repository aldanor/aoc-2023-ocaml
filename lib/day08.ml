open Core

let _A, _AAA, _ZZZ = (Char.to_int 'A', 0, 2)

module KeyTable = struct
  type t = {arr: int array; mutable len: int}

  let create () =
    let arr = Array.create ~len:Int.(26 ** 3) (-1) in
    {arr; len= 0}

  let length t = t.len

  let find_or_add h (c1, c2, c3) =
    let c1, c2, c3 = (Char.to_int c1, Char.to_int c2, Char.to_int c3) in
    let i = ((c1 - _A) * 26 * 26) + ((c2 - _A) * 26) + (c3 - _A) in
    let v = Array.unsafe_get h.arr i in
    if v = -1 then (
      let len = h.len in
      let v = 2 * len in
      (* x2 because we need to store both L/R *)
      Array.unsafe_set h.arr i v ;
      h.len <- len + 1 ;
      v )
    else v
end

module M = struct
  type t = {dirs: int array; arr: int array}

  let parse s =
    let module P = Imports.StreamParser in
    let p = P.create s in
    let dirs = ref [] in
    while not (P.is_newline_u p) do
      let dir = P.parse_char_u p |> Char.equal 'R' |> Bool.to_int in
      dirs := dir :: !dirs
    done ;
    P.skip p 2 ;
    let h = KeyTable.create () in
    let parse_dest i =
      let c1 = P.get_u p ~pos:i in
      let c2 = P.get_u p ~pos:(i + 1) in
      let c3 = P.get_u p ~pos:(i + 2) in
      KeyTable.find_or_add h (c1, c2, c3)
    in
    assert (KeyTable.find_or_add h ('A', 'A', 'A') = _AAA) |> ignore ;
    assert (KeyTable.find_or_add h ('Z', 'Z', 'Z') = _ZZZ) |> ignore ;
    let nodes = ref [] in
    while P.not_eof p do
      let src = parse_dest 0 in
      let left = parse_dest 7 in
      let right = parse_dest 12 in
      nodes := (src, left, right) :: !nodes ;
      P.skip p 17
    done ;
    let arr = Array.create ~len:(KeyTable.length h * 2) (-1) in
    List.iter !nodes ~f:(fun (src, left, right) ->
        arr.(src) <- left ;
        arr.(src + 1) <- right ) ;
    let dirs = Array.of_list_rev !dirs in
    {dirs; arr}

  let part1 {dirs; arr} =
    (* 22411 *)
    let ndir = Array.length dirs in
    let rec steps' i j n =
      if i = _ZZZ then n
      else
        let d = Array.unsafe_get dirs j in
        let j = if not (j = ndir - 1) then j + 1 else 0 in
        let k = if d = 0 then i else i + 1 in
        let i = Array.unsafe_get arr k in
        steps' i j (n + 1)
    in
    steps' _AAA 0 0 |> Int.to_string

  let part2 _ = ""
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
   ZZZ = (ZZZ, ZZZ)" |> run_test ;
  [%expect {| 2 |}]

let%expect_test _ =
  "LLR\n\nAAA = (BBB, BBB)\nBBB = (AAA, ZZZ)\nZZZ = (ZZZ, ZZZ)" |> run_test ;
  [%expect {| 6 |}]

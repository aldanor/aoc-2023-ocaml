open Core

let numbers =
  [ "zero"
  ; "one"
  ; "two"
  ; "three"
  ; "four"
  ; "five"
  ; "six"
  ; "seven"
  ; "eight"
  ; "nine" ]

type trie = Leaf of string * int | Node of trie option array

let rec build_trie = function
  | [] -> failwith "empty list"
  | [(i, x)] -> Leaf (x, i)
  | items ->
      let first_chars =
        List.map ~f:(fun (_, x) -> x.[0]) items
        |> List.dedup_and_sort ~compare:Char.compare
      in
      let arr = Array.create ~len:256 None in
      List.iter first_chars ~f:(fun c ->
          let items =
            List.filter_map items ~f:(fun (i, x) ->
                Option.some_if
                  (Char.equal x.[0] c)
                  (i, String.drop_prefix x 1) )
          in
          arr.(Char.to_int c) <- Some (build_trie items) ) ;
      Node arr

let numbers_trie = build_trie (List.mapi numbers ~f:(fun i x -> (i, x)))

let rec match_trie ?(trie = numbers_trie) s pos =
  if String.length s <= pos then None
  else
    match trie with
    | Leaf (x, j) ->
        if String.is_substring_at s ~pos ~substring:x then Some j else None
    | Node arr ->
        String.get s pos |> Char.to_int |> Array.get arr
        |> Option.bind ~f:(fun trie -> match_trie s (pos + 1) ~trie)

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 54239 *)
    let nan = Char.min_value in
    let digit c = Char.to_int c - Char.to_int '0' in
    let num2 x y = (10 * digit x) + digit y in
    let total, first, last = (ref 0, ref nan, ref nan) in
    String.iter s ~f:(function
      | '0' .. '9' as c ->
          last := c ;
          if Char.(!first = nan) then first := c
      | '\n' ->
          total := !total + num2 !first !last ;
          first := nan
      | _ -> () ) ;
    !total + num2 !first !last |> Int.to_string

  let part2 s =
    (* 55343 *)
    let nan = -1 in
    let digit c = Char.to_int c - Char.to_int '0' in
    let num2 x y = (10 * x) + y in
    let total, first, last = (ref 0, ref nan, ref nan) in
    for i = 0 to String.length s - 1 do
      match String.unsafe_get s i with
      | '0' .. '9' as c ->
          last := digit c ;
          if !first = nan then first := !last
      | '\n' ->
          total := !total + num2 !first !last ;
          first := nan
      | _ -> (
        match match_trie s i with
        | Some j ->
            last := j ;
            if !first = nan then first := j
        | None -> () )
    done ;
    !total + num2 !first !last |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet" |> run_test ~part:1 ;
  [%expect {| 142 |}]

let%expect_test _ =
  "two1nine\n\
   eightwothree\n\
   abcone2threexyz\n\
   xtwone3four\n\
   4nineeightseven2\n\
   zoneight234\n\
   7pqrstsixteen" |> run_test ~part:2 ;
  [%expect {| 281 |}]

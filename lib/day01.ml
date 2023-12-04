open! Stdlib.Printf
open! Base
open! Imports

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

module M = struct
  type t = string

  let parse inputs = inputs

  let part1 inputs =
    (* 54239 *)
    let nan = Char.min_value in
    let digit c = Char.to_int c - Char.to_int '0' in
    let num2 x y = (10 * digit x) + digit y in
    let total, first, last = (ref 0, ref nan, ref nan) in
    String.iter inputs ~f:(function
      | '0' .. '9' as c ->
          last := c ;
          if Char.(!first = nan) then first := c
      | '\n' ->
          total := !total + num2 !first !last ;
          first := nan
      | _ -> () ) ;
    !total + num2 !first !last |> Int.to_string

  let part2 lines =
    (* 55343 *)
    let lines = String.split_lines lines |> List.map ~f:String.to_list in
    let numbers = List.map ~f:String.to_list numbers in
    let numbers_rev = List.map ~f:List.rev numbers in
    let rec zip_shortest xs ys =
      match (xs, ys) with
      | [], _ | _, [] -> []
      | x :: xs, y :: ys -> (x, y) :: zip_shortest xs ys
    in
    let prefix_matches xs ys =
      List.for_all (zip_shortest xs ys) ~f:(fun (x, y) -> Char.equal x y)
    in
    let prefix_findi xs yss =
      List.findi yss ~f:(fun _ ys -> prefix_matches xs ys)
      |> Option.map ~f:fst
    in
    let parse_first_digit chars =
      chars |> List.hd |> Option.find_map ~f:parse_digit
    in
    let rec find_first_digit line numbers =
      let first =
        match parse_first_digit line with
        | None -> prefix_findi line numbers
        | x -> x
      in
      match (first, line) with
      | Some i, _ -> i
      | None, _ :: tail -> find_first_digit tail numbers
      | None, [] -> failwith "no match"
    in
    let first_digit line = find_first_digit line numbers in
    let last_digit line = find_first_digit (List.rev line) numbers_rev in
    let string_code line = (10 * first_digit line) + last_digit line in
    lines |> List.map ~f:string_code
    |> List.fold ~init:0 ~f:( + )
    |> Int.to_string
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
   7pqrstsixteen\n" |> run_test ~part:2 ;
  [%expect {| 281 |}]

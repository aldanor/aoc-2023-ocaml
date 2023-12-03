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
  type t = char list list

  let parse inputs = String.split_lines inputs |> List.map ~f:String.to_list

  let part1 lines =
    (* 54239 *)
    let find_digit chars =
      chars
      |> List.find ~f:Char.is_digit
      |> Option.value ~default:'0' |> Char.to_int
      |> fun c -> c - Char.to_int '0'
    in
    let first_digit line = find_digit line in
    let last_digit line = find_digit (List.rev line) in
    let string_code line = (10 * first_digit line) + last_digit line in
    lines |> List.map ~f:string_code
    |> List.fold ~init:0 ~f:( + )
    |> Int.to_string

  let part2 lines =
    (* 55343 *)
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

let example = ""

let%expect_test _ = run example ; [%expect {| |}]

open Imports
open Core

type matrix = {rows: int array; cols: int array; nrows: int; ncols: int}

let to_string_bin min_width n =
  Pp_binary_ints.Int.make_to_string ~zero_padding:true ~left_padding:true
    ~min_width ~separators:false ~prefix:false () n
  |> String.substr_replace_all ~pattern:"0" ~with_:"."
  |> String.substr_replace_all ~pattern:"1" ~with_:"#"

let print_matrix {rows; cols; nrows; ncols} =
  printf "nrows: %d, ncols: %d\n%!" nrows ncols ;
  let module Bin = Pp_binary_ints.Int in
  printf "Rows:\n%!" ;
  for i = 0 to nrows - 1 do
    printf "%s\n%!" @@ to_string_bin ncols rows.(i)
  done ;
  printf "Cols:\n%!" ;
  for j = 0 to ncols - 1 do
    printf "%s\n%!" @@ to_string_bin nrows cols.(j)
  done

let parse_input s =
  let len, n, pos = (24, String.length s, ref 0) in
  let ms = ref [] in
  while !pos < n do
    let rows, cols = (Array.create ~len 0, Array.create ~len 0) in
    let i, ncols = (ref 0, String.index_from_exn s !pos '\n' - !pos) in
    while !pos < n && Char.(String.unsafe_get s !pos <> '\n') do
      let v_row = ref rows.(!i) in
      for j = 0 to ncols - 1 do
        let v = Char.to_int (String.unsafe_get s !pos) land 1 in
        Array.unsafe_set cols j @@ ((Array.unsafe_get cols j lsl 1) + v) ;
        v_row := (!v_row lsl 1) + v ;
        incr pos
      done ;
      Array.unsafe_set rows !i !v_row ;
      incr pos ;
      incr i
    done ;
    incr pos ;
    ms := {rows; cols; nrows= !i; ncols} :: !ms
  done ;
  !ms

let find_symm ?(verify = false) x n ~smudge =
  let xor1 a b = a lxor b |> Int.popcount in
  let get i = Array.unsafe_get x i in
  let rec f_smudged i mirror_line =
    let f = if smudge then f_non_smudged else f_smudged in
    match mirror_line with
    | None ->
        if i >= n - 1 then None
        else if get i = get (i + 1) then f_smudged (i - 1) (Some i)
        else f (i + 1) None
    | Some j ->
        let k = (2 * j) + 1 - i in
        if i < 0 || k >= n then Some (j + 1)
        else if get i = get k then f_smudged (i - 1) (Some j)
        else f (j + 1) None
  and f_non_smudged i mirror_line =
    match mirror_line with
    | None -> (
        if i >= n - 1 then None
        else
          match xor1 (get i) (get (i + 1)) with
          | 0 -> f_non_smudged (i - 1) (Some i)
          | 1 -> f_smudged (i - 1) (Some i)
          | _ -> f_non_smudged (i + 1) None )
    | Some j -> (
        let k = (2 * j) + 1 - i in
        if i < 0 || k >= n then f_non_smudged (j + 1) None
        else
          match xor1 (get i) (get k) with
          | 0 -> f_non_smudged (i - 1) (Some j)
          | 1 -> f_smudged (i - 1) (Some j)
          | _ -> f_non_smudged (j + 1) None )
  in
  let out =
    (if smudge then f_non_smudged else f_smudged) 0 None
    |> Option.value ~default:0
  in
  ( if out <> 0 && verify then
      let rec verify i j s =
        if i < 0 || j >= n then s
        else verify (i - 1) (j + 1) (s + xor1 x.(i) x.(j))
      in
      let s = verify (out - 1) out 0 in
      match (s, smudge) with
      | 0, false | 1, true -> ()
      | _ -> failwith (sprintf "ns: %d, smudge: %b, out: %d" s smudge out) ) ;
  out

let symm_score m ~smudge =
  let g = find_symm ~smudge in
  match g m.rows m.nrows with 0 -> g m.cols m.ncols | x -> 100 * x

module M = struct
  type t = matrix list

  let parse = parse_input

  let part1 ms =
    (* 33728 *)
    ms
    |> List.map ~f:(fun m -> symm_score m ~smudge:false)
    |> sum_ints |> Int.to_string

  let part2 ms =
    (* 28235 *)
    ms
    |> List.map ~f:(fun m -> symm_score m ~smudge:true)
    |> sum_ints |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "#.##..##.\n\
   ..#.##.#.\n\
   ##......#\n\
   ##......#\n\
   ..#.##.#.\n\
   ..##..##.\n\
   #.#.##.#.\n\n\
   #...##..#\n\
   #....#..#\n\
   ..##..###\n\
   #####.##.\n\
   #####.##.\n\
   ..##..###\n\
   #....#..#" |> run_test ;
  [%expect {| 405 400 |}]

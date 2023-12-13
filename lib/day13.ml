open Core

type matrix = {rows: int array; cols: int array; nrows: int; ncols: int}

let print_matrix {rows; cols; nrows; ncols} =
  printf "nrows: %d, ncols: %d\n%!" nrows ncols ;
  let module Bin = Pp_binary_ints.Int in
  let to_string_bin min_width =
    Pp_binary_ints.Int.make_to_string ~zero_padding:true ~left_padding:true
      ~min_width ~separators:false ~prefix:false ()
  in
  printf "Rows:\n%!" ;
  for i = 0 to nrows - 1 do
    printf "%s\n%!" @@ to_string_bin ncols rows.(i)
  done ;
  printf "Cols:\n%!" ;
  for j = 0 to ncols - 1 do
    printf "%s\n%!" @@ to_string_bin nrows cols.(j)
  done

let parse_input s ~f =
  let len, n, pos = (24, String.length s, ref 0) in
  let rows, cols = (Array.create ~len 0, Array.create ~len 0) in
  while !pos < n do
    Array.fill rows ~pos:0 ~len 0 ;
    Array.fill cols ~pos:0 ~len 0 ;
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
    f {rows; cols; nrows= !i; ncols}
  done

let find_symm x n =
  let rec f i mirror_line =
    match mirror_line with
    | None ->
        if i >= n then None
        else if x.(i) = x.(i + 1) then f (i - 1) (Some i)
        else f (i + 1) None
    | Some j ->
        let k = (2 * j) + 1 - i in
        if i < 0 || k >= n then Some (j + 1)
        else if x.(i) = x.(k) then f (i - 1) (Some j)
        else f (j + 1) None
  in
  f 0 None |> Option.value ~default:0

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 33728 *)
    let a = ref 0 in
    let f m =
      a := !a + (find_symm m.rows m.nrows * 100) + find_symm m.cols m.ncols
    in
    parse_input s ~f ; !a |> Int.to_string

  let part2 _ = ""
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
  [%expect {| 405 |}]

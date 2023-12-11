open Core
open! Imports

let _parse s =
  let n = String.length s in
  let w = String.index_exn s '\n' in
  let h = ((n + 1) / w) - 1 in
  let xs, ys, i = (ref [], ref [], ref 0) in
  for y = 0 to h - 1 do
    for x = 0 to w - 1 do
      let c = String.unsafe_get s !i in
      if Char.(c = '#') then (
        xs := x :: !xs ;
        ys := y :: !ys ) ;
      incr i
    done ;
    incr i
  done ;
  (!xs, !ys, w, h)

let find_gaps xs max =
  let rec loop xs is acc =
    match (xs, is) with
    | _, [] -> acc
    | [], i :: is -> loop [] is (i :: acc)
    | x :: xs, i :: is ->
        if x = i then loop xs is acc
        else if x < i then loop xs (i :: is) acc
        else loop (x :: xs) is (i :: acc)
  in
  loop xs (List.range 0 max) [] |> List.rev

let coords_to_counts xs max =
  let counts = Array.create ~len:max 0 in
  List.iter xs ~f:(fun x -> counts.(x) <- counts.(x) + 1) ;
  counts

let num_gap_crossings xc n =
  let n_left, acc = (ref 0, ref 0) in
  for i = 0 to Array.length xc - 1 do
    let c = xc.(i) in
    if c = 0 then acc := !acc + (!n_left * (n - !n_left))
    else n_left := !n_left + c
  done ;
  !acc

let total_pairwise_distances xc n =
  let n_left, acc, x_prev = (ref 0, ref 0, ref (-1)) in
  for i = 0 to Array.length xc - 1 do
    let c = xc.(i) in
    if c <> 0 then (
      if !x_prev <> -1 then
        acc := !acc + (!n_left * (n - !n_left) * (i - !x_prev)) ;
      x_prev := i ;
      n_left := !n_left + c )
  done ;
  !acc

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 9724940 *)
    let xs, ys, w, h = _parse s in
    let _xs, _ys = (xs, ys) in
    let _w, _h = (w, h) in
    let nx, ny = (List.length xs, List.length ys) in
    let xc = coords_to_counts xs w in
    let yc = coords_to_counts ys h in
    let ngx, ngy = (num_gap_crossings xc nx, num_gap_crossings yc ny) in
    let tpx, tpy =
      (total_pairwise_distances xc nx, total_pairwise_distances yc ny)
    in
    let total = ngx + ngy + tpx + tpy in
    total |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "...#......\n\
   .......#..\n\
   #.........\n\
   ..........\n\
   ......#...\n\
   .#........\n\
   .........#\n\
   ..........\n\
   .......#..\n\
   #...#....." |> run_test ;
  [%expect {| 374 |}]

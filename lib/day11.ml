open Core

type points = {counts: int array; total: int; factor: int}

let count_distances ?(f = None) {counts; total; factor} =
  let n_left, acc, x_prev = (ref 0, ref 0, ref (-1)) in
  let f = Option.value f ~default:factor - 1 in
  for i = 0 to Array.length counts - 1 do
    let c = Array.unsafe_get counts i in
    if c <> 0 then (
      if !x_prev <> -1 then
        acc := !acc + (!n_left * (total - !n_left) * (i - !x_prev)) ;
      x_prev := i ;
      n_left := !n_left + c )
    else acc := !acc + (f * !n_left * (total - !n_left))
  done ;
  !acc

let count_pair_distances ?f xs ys =
  count_distances ~f xs + count_distances ~f ys

module M = struct
  type t = points * points

  let parse s =
    let factor, s =
      if Char.is_digit (String.get s 0) then
        let i = String.index_exn s ' ' in
        let f = String.sub s ~pos:0 ~len:i |> Int.of_string in
        (f, String.sub s ~pos:(i + 1) ~len:(String.length s - i - 1))
      else (1_000_000, s)
    in
    let n = String.length s in
    let w = String.index_exn s '\n' in
    let h = ((n + 1) / w) - 1 in
    let xc = Array.create ~len:w 0 in
    let yc = Array.create ~len:h 0 in
    let i, total = (ref 0, ref 0) in
    for y = 0 to h - 1 do
      for x = 0 to w - 1 do
        let c = String.unsafe_get s !i in
        if Char.(c = '#') then (
          xc.(x) <- xc.(x) + 1 ;
          yc.(y) <- yc.(y) + 1 ;
          incr total ) ;
        incr i
      done ;
      incr i
    done ;
    let total = !total in
    ({counts= xc; total; factor}, {counts= yc; total; factor})

  let part1 (xc, yc) =
    (* 9724940 *)
    count_pair_distances ~f:2 xc yc |> Int.to_string

  let part2 (xc, yc) =
    (* 569052586852 *)
    count_pair_distances xc yc |> Int.to_string
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
   #...#....." |> run_test ~part:1 ;
  [%expect {| 374 |}]

let%expect_test _ =
  "10 ...#......\n\
   .......#..\n\
   #.........\n\
   ..........\n\
   ......#...\n\
   .#........\n\
   .........#\n\
   ..........\n\
   .......#..\n\
   #...#....." |> run_test ~part:2 ;
  [%expect {| 1030 |}]

let%expect_test _ =
  "100 ...#......\n\
   .......#..\n\
   #.........\n\
   ..........\n\
   ......#...\n\
   .#........\n\
   .........#\n\
   ..........\n\
   .......#..\n\
   #...#....." |> run_test ~part:2 ;
  [%expect {| 8410 |}]

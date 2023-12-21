open Core

let count_even ?(start = -1) s steps =
  (* points reachable from the starting point in even number of steps *)
  let start = if start >= 0 then start else String.index_exn s 'S' in
  assert (Char.(String.get s start = 'S' || String.get s start = '.')) ;
  let w = 1 + String.index_exn s '\n' in
  let n = String.length s in
  let visited = Array.create ~len:n false in
  visited.(start) <- true ;
  let offsets = [|-w; -1; 1; w|] in
  let count = ref 0 in
  let frontier = Queue.create () in
  Queue.enqueue frontier (start, steps) ;
  while
    match Queue.dequeue frontier with
    | None -> false
    | Some (_, 0) -> incr count ; true
    | Some (i, steps) ->
        if steps % 2 = 0 then incr count ;
        for j = 0 to 3 do
          let i' = i + offsets.(j) in
          if i' >= 0 && i' < n then
            let c = s.[i'] in
            if (not visited.(i')) && Char.(c = '.' || c = 'S') then (
              Queue.enqueue frontier (i', steps - 1) ;
              visited.(i') <- true )
        done ;
        true
  do
    ()
  done ;
  !count

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 3722 *)
    let steps = if String.length s < 1000 then 6 else 64 in
    count_even s steps |> Int.to_string

  let part2 s =
    (* first, verify input structure which seems to always hold true *)
    (* (note: it won't work on the example input) *)
    let steps = 26501365 in
    (* 614864614526014 *)
    let start = String.index_exn s 'S' in
    let w = String.index_exn s '\n' in
    let len = String.length s in
    let h = (len + 1) / (w + 1) in
    (* it is a square *)
    assert (w = h) ;
    (* side is an odd number *)
    assert (w % 2 = 1) ;
    (* S is in the center *)
    assert (len = (start * 2) + 1) ;
    (* perimeter is empty *)
    for i = 0 to w - 1 do
      assert (Char.(String.get s i = '.')) ;
      assert (Char.(String.get s (i + ((h - 1) * (w + 1))) = '.')) ;
      assert (Char.(String.get s (i * (w + 1)) = '.')) ;
      assert (Char.(String.get s ((i * (w + 1)) + (w - 1)) = '.'))
    done ;
    (* centre row and column are also empty => all distances = taxicab *)
    let d = (w - 1) / 2 in
    (* distance from center to perimeter is odd *)
    assert (d % 2 = 1) ;
    for i = 1 to d do
      assert (Char.(String.get s (start - i) = '.')) ;
      assert (Char.(String.get s (start + i) = '.')) ;
      assert (Char.(String.get s (start - (i * (w + 1))) = '.')) ;
      assert (Char.(String.get s (start + (i * (w + 1))) = '.'))
    done ;
    (* we'll land exactly in the center of the outermost squares *)
    assert ((steps - d) % w = 0) ;
    (* geometric solution *)
    let sq x = x * x in
    let g = (steps / w) - 1 in
    let n_odd_grids = (g / 2 * 2) + 1 |> sq in
    let n_even_grids = (g + 1) / 2 * 2 |> sq in
    let f y x n = count_even ~start:(x + (y * (w + 1))) s n in
    let n_odd = f d d ((w * 2) + 1) in
    let n_even = f d d (w * 2) in
    let e = w - 1 in
    let count_from_midpoints k = f d 0 k + f 0 d k + f d e k + f e d k in
    let count_from_corners k = f e 0 k + f 0 e k + f e e k + f 0 0 k in
    let corners = count_from_midpoints (w - 1) in
    let small = count_from_corners (d - 1) in
    let large = count_from_corners (w + d - 1) in
    let extra = corners + ((g + 1) * small) + (g * large) in
    let ans = (n_odd_grids * n_odd) + (n_even_grids * n_even) + extra in
    ans |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "...........\n\
   .....###.#.\n\
   .###.##..#.\n\
   ..#.#...#..\n\
   ....#.#....\n\
   .##..S####.\n\
   .##..#...#.\n\
   .......##..\n\
   .##.#.####.\n\
   .##..##.##.\n\
   ..........." |> run_test ~part:1 ;
  [%expect {| 16 |}]

open Core

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 3722 *)
    let start = String.index_exn s 'S' in
    let w = 1 + String.index_exn s '\n' in
    let n = String.length s in
    let visited = Array.create ~len:n false in
    visited.(start) <- true ;
    let offsets = [|-w; -1; 1; w|] in
    let steps = if n < 1000 then 6 else 64 in
    let frontier = Queue.create () in
    Queue.enqueue frontier start ;
    let total = ref 1 in
    for j = 1 to steps do
      let count = ref 0 in
      let k = Queue.length frontier in
      for _ = 1 to k do
        match Queue.dequeue frontier with
        | None -> ()
        | Some i ->
            for j = 0 to 3 do
              let i' = i + Array.unsafe_get offsets j in
              if i' >= 0 && i' < n then
                if
                  (not (Array.unsafe_get visited i'))
                  && Char.(String.unsafe_get s i' = '.')
                then (
                  Queue.enqueue frontier i' ;
                  incr count ;
                  Array.unsafe_set visited i' true )
            done
      done ;
      if j % 2 = 0 then total := !total + !count
    done ;
    !total |> Int.to_string

  let part2 _ = ""
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
   ..........." |> run_test ;
  [%expect {| 16 |}]

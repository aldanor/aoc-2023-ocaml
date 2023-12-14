open Core

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 110090 *)
    let w = String.index_exn s '\n' in
    let h = (String.length s + 1) / (w + 1) in
    let a = Array.create ~len:w 0 in
    let count, load = (ref 0, ref 0) in
    for i = 0 to h - 1 do
      let offset = i * (w + 1) in
      for j = 0 to w - 1 do
        match s.[offset + j] with
        | 'O' ->
            incr count ;
            load := !load + a.(j) ;
            a.(j) <- a.(j) + 1
        | '#' -> a.(j) <- i + 1
        | _ -> ()
      done
    done ;
    (!count * h) - !load |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "O....#....\n\
   O.OO#....#\n\
   .....##...\n\
   OO.#O....O\n\
   .O.....O#.\n\
   O.#..O.#.#\n\
   ..O..#O..O\n\
   .......O..\n\
   #....###..\n\
   #OO..#...." |> run_test ;
  [%expect {| 136 |}]

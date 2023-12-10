open! Core
open! Imports

type dir = Up | Right | Down | Left [@@deriving show {with_path= false}]

type state = {mutable pos: int; mutable dir: dir}
[@@deriving show {with_path= false}]

let dir_to_offset w d =
  match d with Up -> -w | Right -> 1 | Down -> w | Left -> -1

(* returns offset to position and the new direction *)
let next_dir d c =
  match (c, d) with
  | '|', Down -> Down
  | '|', Up -> Up
  | '-', Right -> Right
  | '-', Left -> Left
  | 'L', Down -> Right
  | 'L', Left -> Up
  | 'F', Left -> Down
  | 'F', Up -> Right
  | 'J', Down -> Left
  | 'J', Right -> Up
  | '7', Right -> Down
  | '7', Up -> Left
  | c, d -> failwith (sprintf "invalid char=%c and dir=%s" c (show_dir d))

let advance_state map w (s : state) =
  let offset = dir_to_offset w s.dir in
  s.pos <- s.pos + offset ;
  s.dir <- next_dir s.dir (String.get map s.pos)

let advance_states map w (s1 : state) (s2 : state) =
  let p1, p2 = (s1.pos, s2.pos) in
  advance_state map w s1 ;
  advance_state map w s2 ;
  p1 = p2 || (p1 = s2.pos && p2 = s1.pos)

let find_start map =
  let w = 1 + String.index_exn map '\n' in
  let start_pos = String.index_exn map 'S' in
  let start_options =
    [ (Up, -w, "|F7", '|')
    ; (Right, 1, "-J7", '-')
    ; (Down, w, "|LJ", '|')
    ; (Left, -1, "-FL", '-') ]
  in
  match
    List.filter_map start_options ~f:(fun (d, o, cs, _) ->
        let pos = start_pos + o in
        let c = String.get map pos in
        if not (String.contains cs c) then None
        else
          let dir = next_dir d c in
          Some {pos; dir} )
  with
  | [a; b] -> (w, (a, b))
  | _ -> failwith "invalid start dirs"

let dump_map_and_two_states map s1 s2 =
  let map =
    String.mapi map ~f:(fun i c ->
        if i = s1.pos then '1' else if i = s2.pos then '2' else c )
  in
  printf "%s\n" map

module M = struct
  type t = string

  let parse s = s

  let part1 map =
    let rec traverse_loop map w n s1 s2 =
      let loop_closed = advance_states map w s1 s2 in
      if loop_closed then n else traverse_loop map w (n + 1) s1 s2
    in
    let w, (s1, s2) = find_start map in
    traverse_loop map w 1 s1 s2 |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..." |> run_test ;
  [%expect {| 8 |}]

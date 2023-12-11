open! Core
open! Imports

type dir = Up | Right | Down | Left
[@@deriving show {with_path= false}, eq, enum]

let rev_dir = function
  | Up -> Down
  | Right -> Left
  | Down -> Up
  | Left -> Right

type state = {pos: int; dir: dir; prev_dir: dir}
[@@deriving show {with_path= false}]

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

module Map = struct
  type t = {map: Bytes.t; start: int; dir: dir; prev_dir: dir; w: int}

  let parse s =
    let w = 1 + String.index_exn s '\n' in
    let start = String.index_exn s 'S' in
    let n = String.length s in
    let map = Bytes.of_string s in
    let infer_char d1 d2 =
      match (d1, d2) with
      | Up, Right -> 'L'
      | Up, Down -> '|'
      | Up, Left -> 'J'
      | Right, Down -> 'F'
      | Right, Left -> '-'
      | Down, Left -> '7'
      | _ -> failwith "invalid start dirs"
    in
    let options =
      [ (Up, -w, "|F7")
      ; (Right, 1, "-J7")
      ; (Down, w, "|LJ")
      ; (Left, -1, "-FL") ]
    in
    let dirs =
      List.filter_map options ~f:(fun (d, o, cs) ->
          let pos = start + o in
          if pos < 0 || pos >= n then None
          else if not (pos |> Bytes.get map |> String.contains cs) then None
          else Some d )
    in
    match dirs with
    | [d1; d2] ->
        let c = infer_char d1 d2 in
        Bytes.set map start c ;
        let dir, prev_dir = (d1, rev_dir d2) in
        {map; start; dir; prev_dir; w}
    | _ -> failwith "invalid start dirs"

  let length m = Bytes.length m.map

  let width m = m.w

  let start m = {pos= m.start; dir= m.dir; prev_dir= m.prev_dir}

  let advance m (s : state) =
    let offset =
      match s.dir with Up -> -m.w | Right -> 1 | Down -> m.w | Left -> -1
    in
    let pos = s.pos + offset in
    let prev_dir = s.dir in
    let dir = next_dir s.dir (Bytes.unsafe_get m.map pos) in
    {pos; dir; prev_dir}

  let traverse_loop m ~f =
    let rec loop' s =
      f s ;
      let s = advance m s in
      if s.pos <> m.start then loop' s
    in
    start m |> loop'

  let left_right m pos dir =
    match dir with
    | Up -> (pos - 1, pos + 1)
    | Right -> (pos - m.w, pos + m.w)
    | Down -> (pos + 1, pos - 1)
    | Left -> (pos + m.w, pos - m.w)

  let is_outside m pos =
    let n = Bytes.length m.map in
    pos < 0 || pos >= n || Char.('\n' = Bytes.get m.map pos)

  let map_left_right m ~f (s : state) =
    let l, r = left_right m s.pos s.dir in
    f l true ;
    f r false ;
    if not (equal_dir s.dir s.prev_dir) then (
      let l, r = left_right m s.pos s.prev_dir in
      f l true ; f r false )
end

module M = struct
  type t = string

  let parse s = s

  let part1 map =
    let m, n = (Map.parse map, ref 0) in
    Map.traverse_loop m ~f:(fun _ -> incr n) ;
    !n / 2 |> Int.to_string

  let part2 map =
    (* 383 *)
    assert (Int.(dir_to_enum Up = 0)) ;
    assert (Int.(dir_to_enum Right = 1)) ;
    assert (Int.(dir_to_enum Down = 2)) ;
    assert (Int.(dir_to_enum Left = 3)) ;
    let m = Map.parse map in
    (* traverse the loop, collect loop nodes, mark them with 1 *)
    let b = Array.create_local ~len:(Map.length m) 0 in
    let loop_nodes = ref [] in
    Map.traverse_loop m ~f:(fun s ->
        Array.unsafe_set b s.pos 1 ;
        loop_nodes := s :: !loop_nodes ) ;
    let n, w = (Map.length m, Map.width m) in
    (* fill out from upperpost right corner, mark as -1 *)
    let rec traverse_flood' pos area is_outside =
      match pos with
      | [] -> (area, is_outside)
      | i :: tl when i < 0 || i >= n -> traverse_flood' tl area true
      | i :: tl -> (
        match Array.unsafe_get b i with
        | 0 ->
            Array.unsafe_set b i (-1) ;
            traverse_flood'
              ((i - w) :: (i - 1) :: (i + 1) :: (i + w) :: tl)
              (area + 1) is_outside
        (* '2' means "we've marked it as outward", see below *)
        | x -> traverse_flood' tl area (is_outside || x = 2) )
    in
    let traverse_flood pos =
      let area, is_outside = traverse_flood' [pos] 0 false in
      if not is_outside then area else 0
    in
    let _ = traverse_flood w in
    (* now we can determine whether left or right is outward *)
    let offsets_l, offsets_r = ([|-1; -w; 1; w|], [|1; w; -1; -w|]) in
    let is_out pos = pos < 0 || pos >= n || Array.unsafe_get b pos = -1 in
    let is_out_l pos d = pos + Array.unsafe_get offsets_l d |> is_out in
    let is_out_r pos d = pos + Array.unsafe_get offsets_r d |> is_out in
    let rec is_out_left' = function
      | [] -> failwith "unable to determine outward side"
      | s :: tl ->
          let p, d = (dir_to_enum s.prev_dir, dir_to_enum s.dir) in
          if is_out_l s.pos p || is_out_l s.pos d then true
          else if is_out_r s.pos p || is_out_r s.pos d then false
          else is_out_left' tl
    in
    let is_out_left = is_out_left' !loop_nodes in
    (* now let's go and fill empty slots on outward side with 2 *)
    let mark pos =
      if pos >= 0 && pos < n && Array.unsafe_get b pos = 0 then
        Array.unsafe_set b pos 2
    in
    let offsets = if is_out_left then offsets_l else offsets_r in
    let rec mark_outward_side' = function
      | [] -> ()
      | s :: tl ->
          mark (s.pos + Array.unsafe_get offsets (dir_to_enum s.prev_dir)) ;
          mark (s.pos + Array.unsafe_get offsets (dir_to_enum s.dir)) ;
          mark_outward_side' tl
    in
    mark_outward_side' !loop_nodes ;
    (* now we're ready to do the flood fill *)
    let rec compute_area min_pos area =
      if min_pos >= n then area
      else
        let area =
          if Array.get b min_pos = 0 then area + traverse_flood min_pos
          else area
        in
        compute_area (min_pos + 1) area
    in
    compute_area 0 0 |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "..F7.\n.FJ|.\nSJ.L7\n|F--J\nLJ..." |> run_test ~part:1 ;
  [%expect {| 8 |}]

let%expect_test _ =
  "...........\n\
   .S-------7.\n\
   .|F-----7|.\n\
   .||.....||.\n\
   .||.....||.\n\
   .|L-7.F-J|.\n\
   .|..|.|..|.\n\
   .L--J.L--J.\n\
   ..........." |> run_test ~part:2 ;
  [%expect {| 4 |}]

let%expect_test _ =
  "..........\n\
   .S------7.\n\
   .|F----7|.\n\
   .||OOOO||.\n\
   .||OOOO||.\n\
   .|L-7F-J|.\n\
   .|II||II|.\n\
   .L--JL--J.\n\
   .........." |> run_test ~part:2 ;
  [%expect {| 4 |}]

let%expect_test _ =
  ".F----7F7F7F7F-7....\n\
   .|F--7||||||||FJ....\n\
   .||.FJ||||||||L7....\n\
   FJL7L7LJLJ||LJ.L-7..\n\
   L--J.L7...LJS7F-7L7.\n\
   ....F-J..F7FJ|L7L7L7\n\
   ....L7.F7||L7|.L7L7|\n\
   .....|FJLJ|FJ|F7|.LJ\n\
   ....FJL-7.||.||||...\n\
   ....L---J.LJ.LJLJ..." |> run_test ~part:2 ;
  [%expect {| 8 |}]

let%expect_test _ =
  "FF7FSF7F7F7F7F7F---7\n\
   L|LJ||||||||||||F--J\n\
   FL-7LJLJ||||||LJL-77\n\
   F--JF--7||LJLJ7F7FJ-\n\
   L---JF-JLJ.||-FJLJJ7\n\
   |F|F-JF---7F7-L7L|7|\n\
   |FFJF7L7F-JF7|JL---7\n\
   7-L-JL7||F7|L7F-7F7|\n\
   L.L7LFJ|||||FJL7||LJ\n\
   L7JLJL-JLJLJL--JLJ.L" |> run_test ~part:2 ;
  [%expect {| 10 |}]

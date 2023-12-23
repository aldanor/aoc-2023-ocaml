open Core

type maze = {s: string; w: int; h: int; o: int}
[@@deriving show {with_path= false}]

let is_free = Char.( = ) '.'

let is_wall = Char.( = ) '#'

let is_slope = function '>' | '<' | '^' | 'v' -> true | _ -> false

let offsets m = [|-m.o; -1; 1; m.o|]

let start m = m.o + 1

let finish m = start m + (m.o * (m.h - 1)) + m.w - 1

let next_options m =
  [| [|(0, -m.o); (1, 1); (3, -1)|]
   ; [|(1, 1); (0, -m.o); (2, m.o)|]
   ; [|(2, m.o); (1, 1); (3, -1)|]
   ; [|(3, -1); (0, -m.o); (2, m.o)|] |]

let find_forks ?(all = false) m =
  (* since the maze has a particular structure, verify it, why not *)
  let offsets, start = (offsets m, start m) in
  let row = ref start in
  let check_slopes c c_out c_in =
    match c with
    | c when Char.(c = c_out) -> 1
    | c when Char.(c = c_in) -> 0
    | '#' -> 0
    | _ -> assert false
  in
  let forks = ref [] in
  for _ = 0 to m.h - 1 do
    for x = 0 to m.w - 1 do
      let k = !row + x in
      if not (is_wall m.s.[k]) then (
        let n_free =
          Array.count offsets ~f:(fun d -> not (is_wall m.s.[k + d]))
        in
        (* no dead ends *)
        assert (n_free >= 2) ;
        if n_free >= 3 then
          if all then forks := k :: !forks
          else
            let n = check_slopes m.s.[k - m.o] '^' 'v' in
            let w = check_slopes m.s.[k - 1] '<' '>' in
            let e = check_slopes m.s.[k + 1] '>' '<' in
            let s = check_slopes m.s.[k + m.o] 'v' '^' in
            let n_exits = n + w + e + s in
            if n_exits >= 2 then (
              assert (n_exits = 2) ;
              ( match (n, e, s, w) with
              | _, 1, 1, _ -> () (* forks are always of "SE" type *)
              | _ -> assert false ) ;
              forks := k :: !forks ) )
    done ;
    row := !row + m.o
  done ;
  !forks

let dump_graph edges =
  printf "digraph maze {\n" ;
  Array.iteri edges ~f:(fun i es ->
      List.iter es ~f:(fun (j, d) ->
          printf " a%d -> a%d [label=\"%d\"];\n" i j d ) ) ;
  printf "}\n"

let connect_forks m forks =
  (* directions are n=0, e=1, s=2, w=3 *)
  let fork_rev_map = Array.create ~len:(String.length m.s) (-1) in
  let nodes = start m :: finish m :: forks |> Array.of_list in
  Array.iteri nodes ~f:(fun i f -> fork_rev_map.(f) <- i) ;
  let rev_slope, next = ([|'v'; '<'; '^'; '>'|], next_options m) in
  let rec traverse k dir dist =
    let j = fork_rev_map.(k) in
    if j = -1 || dist = 0 then
      let dir', d =
        Array.find_exn next.(dir) ~f:(fun (dir, d) ->
            let c = m.s.[k + d] in
            (not (is_wall c)) && not Char.(c = rev_slope.(dir)) )
      in
      traverse (k + d) dir' (dist + 1)
    else (j, dist)
  in
  let edges = Array.init (Array.length nodes) ~f:(fun _ -> []) in
  for i = 0 to Array.length nodes - 1 do
    if i <> 1 then
      for dir = 1 to 2 do
        edges.(i) <- traverse nodes.(i) dir 0 :: edges.(i)
      done
  done ;
  edges

module M = struct
  type t = maze

  let parse s =
    let o = 1 + String.index_exn s '\n' in
    let w = o - 3 in
    let n = String.length s in
    let h = ((n + 1) / o) - 2 in
    {s; w; h; o}

  let part1 m =
    (* 2406 *)
    let forks = find_forks m in
    let edges = connect_forks m forks in
    let longest = Array.create ~len:(Array.length edges) 0 in
    let frontier = Queue.create () in
    Queue.enqueue frontier 0 ;
    while
      match Queue.dequeue frontier with
      | None -> false
      | Some i ->
          let dist = longest.(i) in
          List.iter edges.(i) ~f:(fun (j, d) ->
              let should_add = longest.(j) = 0 && j <> 1 in
              longest.(j) <- max longest.(j) (dist + d) ;
              if should_add then Queue.enqueue frontier j ) ;
          true
    do
      ()
    done ;
    2 + longest.(1) |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "#.#####################\n\
   #.......#########...###\n\
   #######.#########.#.###\n\
   ###.....#.>.>.###.#.###\n\
   ###v#####.#v#.###.#.###\n\
   ###.>...#.#.#.....#...#\n\
   ###v###.#.#.#########.#\n\
   ###...#.#.#.......#...#\n\
   #####.#.#.#######.#.###\n\
   #.....#.#.#.......#...#\n\
   #.#####.#.#.#########v#\n\
   #.#...#...#...###...>.#\n\
   #.#.#v#######v###.###v#\n\
   #...#.>.#...>.>.#.###.#\n\
   #####v#.#.###v#.#.###.#\n\
   #.....#...#...#.#.#...#\n\
   #.#########.###.#.#.###\n\
   #...###...#...#...#.###\n\
   ###.###.#.###v#####v###\n\
   #...#...#.#.>.>.#.>.###\n\
   #.###.###.#.###.#.#v###\n\
   #.....###...###...#...#\n\
   #####################.#" |> run_test ;
  [%expect {| 94 |}]

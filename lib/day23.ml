open Core

type maze = {s: string; w: int; h: int; o: int}
[@@deriving show {with_path= false}]

let is_free = Char.( = ) '.'

let is_wall = Char.( = ) '#'

let is_slope = function '>' | '<' | '^' | 'v' -> true | _ -> false

(* let offsets m = [|-m.o; -1; 1; m.o|] *)
let offsets m = [|-m.o; 1; m.o; -1|]

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

let dump_graph ?(undirected = false) edges =
  printf "digraph maze {\n" ;
  printf "  graph[layout = neato]\n" ;
  Array.iteri edges ~f:(fun i es ->
      printf "  a%d;\n" i ;
      List.iter es ~f:(fun (j, d) ->
          if not undirected then printf " a%d -> a%d[label=\"%d\"];\n" i j d
          else if i < j then
            printf "  a%d -> a%d[label=\"%d\",dir=\"none\"];\n" i j d
          else () ) ) ;
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

let connect_forks_undirected m forks =
  (* directions are n=0, e=1, s=2, w=3 *)
  let fork_rev_map = Array.create ~len:(String.length m.s) (-1) in
  let start, finish = (start m, finish m) in
  let nodes = start :: finish :: forks |> Array.of_list in
  Array.iteri nodes ~f:(fun i f -> fork_rev_map.(f) <- i) ;
  let next = next_options m in
  let rec traverse k dir dist =
    let j = fork_rev_map.(k) in
    if j = -1 || dist = 0 then
      let dir', d =
        Array.find_exn next.(dir) ~f:(fun (_, d) ->
            not (is_wall (String.get m.s (k + d))) )
      in
      traverse (k + d) dir' (dist + 1)
    else (j, dir, dist)
  in
  let edges = Array.init (Array.length nodes) ~f:(fun _ -> []) in
  let offsets = offsets m in
  for i = 2 to Array.length nodes - 1 do
    for dir = 0 to 3 do
      let d = offsets.(dir) in
      if not (is_wall (String.get m.s (nodes.(i) + d))) then
        if not (List.exists edges.(i) ~f:(fun (_, _, d) -> d = dir)) then (
          let j, dst_dir, dist = traverse (nodes.(i) + d) dir 1 in
          edges.(i) <- (j, dist, dir) :: edges.(i) ;
          edges.(j) <- (i, dist, (dst_dir + 2) mod 4) :: edges.(j) )
    done
  done ;
  let edges =
    edges |> Array.map ~f:(List.map ~f:(fun (i, d, _) -> (i, d)))
  in
  (* fold edges between same destinations *)
  let edges =
    Array.map edges ~f:(fun es ->
        let m = Int.Table.create () in
        List.iter es ~f:(fun (i, d) ->
            let v =
              Hashtbl.find m i
              |> Option.map ~f:(max d)
              |> Option.value ~default:d
            in
            Hashtbl.set m ~key:i ~data:v ) ;
        Hashtbl.to_alist m )
  in
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

  let part2 s =
    (* 6630 *)
    let forks = find_forks s ~all:true in
    let edges = connect_forks_undirected s forks in
    let longest = ref 0 in
    let frontier = Queue.create () in
    Queue.enqueue frontier (0, 1, 0) ;
    while
      match Queue.dequeue frontier with
      | Some (1, _, dist) ->
          longest := max !longest dist ;
          true
      | Some (i, seen, dist) ->
          List.iter edges.(i) ~f:(fun (j, d) ->
              let mask = 1 lsl j in
              if seen land mask = 0 then
                Queue.enqueue frontier (j, seen lor mask, dist + d) ) ;
          true
      | None -> false
    do
      ()
    done ;
    2 + !longest |> Int.to_string
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
  [%expect {| 94 154 |}]

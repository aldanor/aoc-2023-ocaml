open Core

type node = {id: int; mutable edges: int list; name: string}
[@@deriving show {with_path= false}]

module IntPair = struct
  module T = struct
    type t = int * int

    let compare x y = Tuple2.compare ~cmp1:Int.compare ~cmp2:Int.compare x y

    let sexp_of_t = Tuple2.sexp_of_t Int.sexp_of_t Int.sexp_of_t

    let t_of_sexp = Tuple2.t_of_sexp Int.t_of_sexp Int.t_of_sexp

    let hash = Hashtbl.hash
  end

  include T
  include Comparable.Make (T)
end

let find_furthest_node g i =
  (* bfs-expand from i, return last node visited *)
  let n = Array.length g in
  let seen = Array.create ~len:n false in
  let queue = Queue.create () in
  Queue.enqueue queue i ;
  seen.(i) <- true ;
  let last = ref i in
  while not (Queue.is_empty queue) do
    let i = Queue.dequeue_exn queue in
    last := i ;
    List.iter g.(i).edges ~f:(fun j ->
        if not seen.(j) then (
          seen.(j) <- true ;
          Queue.enqueue queue j ) )
  done ;
  !last

let place_nodes_1d g l r =
  (* bfs-expand from l and r simultaneously, assign [0;1] values to all
     nodes, where 0=L, 1=R, all other nodes get assigned values proportional
     to distance from either L or R, depending which one gets hit first *)
  let n = Array.length g in
  let dist = Array.create ~len:n 0 in
  dist.(l) <- -1 ;
  dist.(r) <- 1 ;
  let queue = Queue.create () in
  Queue.enqueue queue (l, false) ;
  Queue.enqueue queue (r, true) ;
  while not (Queue.is_empty queue) do
    let i, right = Queue.dequeue_exn queue in
    List.iter g.(i).edges ~f:(fun j ->
        if dist.(j) = 0 then (
          dist.(j) <- (dist.(i) + if right then 1 else -1) ;
          Queue.enqueue queue (j, right) ) )
  done ;
  let ld = (Array.fold dist ~init:Int.max_value ~f:min |> float) *. 2.0 in
  let rd = (Array.fold dist ~init:Int.min_value ~f:max |> float) *. 2.0 in
  let pos =
    Array.map dist ~f:(fun d ->
        let d = float d in
        Float.(if d > 0.0 then 1.0 - ((d - 1.0) / rd) else (d + 1.0) / ld) )
  in
  pos

let adjust_node_pos g l r pos =
  (* update positions of all nodes, average them with neighbors *)
  let out = Array.copy pos in
  let n = Array.length g in
  for i = 0 to n - 1 do
    let sum =
      List.fold g.(i).edges ~init:0.0 ~f:(fun sum j -> sum +. pos.(j))
    in
    let k = List.length g.(i).edges |> float in
    let p = pos.(i) in
    let is_root = i = l || i = r in
    (* for root nodes, allow them to move but 2x slower *)
    (* if we don't do this, they will have too much tension *)
    let new_pos = Float.((sum + p) / (k + 1.0)) in
    out.(i) <- (if not is_root then new_pos else Float.((p + new_pos) / 2.0))
  done ;
  out

let sort_edges g pos =
  (* sort the edges by decreasing length given 1-D positions of nodes *)
  let edges = ref [] in
  let n = Array.length g in
  for i = 0 to n - 1 do
    List.iter g.(i).edges ~f:(fun j ->
        if i < j then
          let d = Float.(abs (pos.(i) - pos.(j))) in
          edges := ((i, j), d) :: !edges )
  done ;
  List.sort !edges ~compare:(fun (_, d1) (_, d2) -> Float.compare d2 d1)
  |> List.map ~f:fst

let find_component_sizes ?(banned = []) g =
  (* find sizes of all components in the graph, excluding banned edges *)
  let banned = List.concat_map banned ~f:(fun (i, j) -> [(i, j); (j, i)]) in
  let is_banned i j = List.mem banned (i, j) ~equal:IntPair.equal in
  let n = Array.length g in
  let seen = Array.create ~len:n false in
  let rec dfs i =
    if not seen.(i) then (
      seen.(i) <- true ;
      1
      + List.fold_left g.(i).edges ~init:0 ~f:(fun acc j ->
            if (not seen.(j)) && not (is_banned i j) then acc + dfs j
            else acc ) )
    else 0
  in
  let sizes = ref [] in
  for i = 0 to n - 1 do
    if not seen.(i) then
      let size = dfs i in
      sizes := size :: !sizes
  done ;
  !sizes

let pick_pairs lst =
  (* helper function to pick all 2-element subsets of a list *)
  let rec aux acc = function
    | [] -> acc
    | x :: xs ->
        let pairs = List.map ~f:(fun y -> [x; y]) xs in
        aux (acc @ pairs) xs
  in
  aux [] lst

let plot_graph ?(highlight = []) ?(label = false) ?node_pos
    ?(pos_colors = ((0.5, 1.0, 0.5), (0.5, 0.5, 1.0))) ?(color_edges = false)
    g =
  (* dump the graph to stdout in dot format *)
  let floats_to_hex (r, g, b) =
    let float_to_uint8 f =
      Float.round_nearest (f *. 255.0) |> Int.of_float
    in
    let r = float_to_uint8 r in
    let g = float_to_uint8 g in
    let b = float_to_uint8 b in
    sprintf "\"#%02x%02x%02x\"" r g b
  in
  let mix_colors (r1, g1, b1) (r2, g2, b2) w =
    let mix a b = (a *. w) +. (b *. (1.0 -. w)) in
    (mix r1 r2, mix g1 g2, mix b1 b2)
  in
  printf "digraph g {\n" ;
  printf " graph [layout=neato];\n" ;
  let node_size = if label then "0.20" else "0.10" in
  printf
    " node [fontsize=7, margin=0, fixedsize=true, width=%s, height=%s];\n"
    node_size node_size ;
  printf " edge [fontsize=7, fontcolor=gray];\n" ;
  let pos_to_color p =
    mix_colors (fst pos_colors) (snd pos_colors) p |> floats_to_hex
  in
  for i = 0 to Array.length g - 1 do
    let fillcolor =
      match List.Assoc.find highlight i ~equal with
      | None -> (
        match node_pos with
        | None -> "white"
        | Some pos -> pos_to_color pos.(i) )
      | Some color -> color
    in
    printf " %s [fillcolor=%s, style=filled" g.(i).name fillcolor ;
    if not label then printf ", label=\"\"" ;
    printf "];\n" ;
    let maxdist =
      match node_pos with
      | Some pos ->
          Array.foldi pos ~init:0.0 ~f:(fun i m p ->
              Float.(
                List.fold g.(i).edges ~init:m ~f:(fun m j ->
                    max m (abs (p - pos.(j))) )
                |> max m ) )
      | None -> 0.0
    in
    List.iter g.(i).edges ~f:(fun j ->
        if i < j then
          let d =
            match node_pos with
            | Some pos -> Float.(abs (pos.(i) - pos.(j)))
            | None -> 0.0
          in
          let color =
            if Float.(maxdist = 0.0) || not color_edges then
              floats_to_hex (0.5, 0.5, 0.5)
            else
              mix_colors (1.0, 0.0, 0.0) (0.85, 0.85, 0.85) (d /. maxdist)
              |> floats_to_hex
          in
          printf " %s -> %s [dir=\"none\",color=%s,label=\"%.4f\"];\n"
            g.(i).name g.(j).name color d )
  done ;
  printf "}\n"

module M = struct
  type t = node array

  let parse s =
    let module P = Imports.StreamParser in
    let p = P.create s in
    let node_map = Array.create ~len:(26 * 26 * 26) (-1) in
    let n_nodes = ref 0 in
    let letter c = Char.to_int c - Char.to_int 'a' in
    let nodes =
      Array.init (26 * 26 * 26) ~f:(fun _ -> {id= -1; edges= []; name= ""})
    in
    let parse_node () =
      let c2, c1, c0 =
        (P.parse_char_u p, P.parse_char_u p, P.parse_char_u p)
      in
      let i = (letter c0 * 26 * 26) + (letter c1 * 26) + letter c2 in
      let name = String.of_char_list [c0; c1; c2] in
      match node_map.(i) with
      | -1 ->
          let id = !n_nodes in
          node_map.(i) <- id ;
          nodes.(id) <- {id; edges= []; name} ;
          incr n_nodes ;
          id
      | id -> id
    in
    while P.not_eof p do
      let i = parse_node () in
      P.skip p 1 ;
      while P.not_newline p do
        P.skip p 1 ;
        let j = parse_node () in
        nodes.(i).edges <- j :: nodes.(i).edges ;
        nodes.(j).edges <- i :: nodes.(j).edges
      done ;
      P.skip p 1
    done ;
    nodes |> Array.sub ~pos:0 ~len:!n_nodes

  let part1 g =
    (* 543256 *)
    (* L = furthest node from the first node *)
    let l = find_furthest_node g 0 in
    (* R = furthest node from L *)
    let r = find_furthest_node g l in
    (* assign initial node 1-D positions *)
    let pos = ref (place_nodes_1d g l r) in
    (* iterate a few time to let clusters settle *)
    for _ = 1 to 3 do
      pos := adjust_node_pos g l r !pos
    done ;
    (* edges that we've already tried to exclude *)
    let edges, ans = (ref [], ref 0) in
    while !ans = 0 do
      (* sort edges by decreasing length *)
      let sorted_edges = sort_edges g !pos in
      (* build new banlists *)
      let banlists =
        if List.is_empty !edges then (
          (* when starting out, just pick the first longest edges *)
          edges := List.sub sorted_edges ~pos:0 ~len:3 ;
          [!edges] )
        else
          (* if continuing, pick the first edge we haven't seen yet *)
          let edge =
            List.find_exn sorted_edges ~f:(fun e ->
                not (List.mem !edges e ~equal:IntPair.equal) )
          in
          (* join all 2-pairs of existing edges with the new edge *)
          let to_check =
            List.map (pick_pairs !edges) ~f:(fun e -> edge :: e)
          in
          edges := edge :: !edges ;
          to_check
      in
      (* find component sizes for each banlist *)
      List.iter banlists ~f:(fun banned ->
          let sizes = find_component_sizes ~banned g in
          match sizes with [a; b] when !ans = 0 -> ans := a * b | _ -> () )
    done ;
    !ans |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "jqt: rhn xhk nvd\n\
   rsh: frs pzl lsr\n\
   xhk: hfx\n\
   cmg: qnr nvd lhk bvb\n\
   rhn: xhk bvb hfx\n\
   bvb: xhk hfx\n\
   pzl: lsr hfx nvd\n\
   qnr: nvd\n\
   ntq: jqt hfx bvb xhk\n\
   nvd: lhk\n\
   lsr: lhk\n\
   rzs: qnr cmg lsr rsh\n\
   frs: qnr lhk lsr" |> run_test ;
  [%expect {| 54 |}]

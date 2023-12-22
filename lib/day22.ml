open Core

(* actual length is len + 1; rot = x=0, y=1, z=2 *)
type brick = {x: int; y: int; z: int; len: int; rot: int}
[@@deriving show {with_path= false}]

type graph =
  {zpos: int array; supports: int list array; supported_by: int list array}
[@@deriving show {with_path= false}]

let find_supports bricks =
  let height_map = Array.create ~len:121 0 in
  let top_bricks = Array.create ~len:121 (-1) in
  let n = Array.length bricks in
  let supports = Array.init n ~f:(fun _ -> []) in
  let supported_by = Array.init n ~f:(fun _ -> []) in
  let zpos = Array.create ~len:n 0 in
  let process_brick i b =
    let h = ref 0 in
    let pos = (b.y * 11) + b.x in
    let offset, hlen, dh =
      match b.rot with
      | 0 -> (1, b.len, 1)
      | 1 -> (11, b.len, 1)
      | _ -> (0, 0, b.len + 1)
    in
    let k = ref pos in
    for _ = 0 to hlen do
      h := max !h height_map.(!k) ;
      k := !k + offset
    done ;
    let h, k = (!h, ref pos) in
    zpos.(i) <- h ;
    for _ = 0 to hlen do
      ( if height_map.(!k) = h && h <> 0 then
          let t = top_bricks.(!k) in
          match List.hd supports.(t) with
          | Some x when x = i -> ()
          | _ ->
              supports.(t) <- i :: supports.(t) ;
              supported_by.(i) <- t :: supported_by.(i) ) ;
      height_map.(!k) <- h + dh ;
      top_bricks.(!k) <- i ;
      k := !k + offset
    done
  in
  Array.iteri bricks ~f:process_brick ;
  {zpos; supports; supported_by}

let lca_pair g i j =
  (* lowest common ancestor *)
  let heap =
    Pairing_heap.create
      ~cmp:(fun (i1, h1, _) (i2, h2, _) ->
        (* highest z-pos first, then order by index *)
        let c = compare h2 h1 in
        if c <> 0 then c else compare i1 i2 )
      ()
  in
  (* add both nodes to the heap, with a flag to indicate which node they came
     from; this flag will propagate down to children *)
  Pairing_heap.add heap (i, g.zpos.(i), true) ;
  Pairing_heap.add heap (j, g.zpos.(j), false) ;
  let lca = ref None in
  while
    (* pick a node with highest z-coordinate from the heap *)
    match Pairing_heap.pop heap with
    (* can't find, gg *)
    | None -> false
    | Some (i, _, p) -> (
      match Pairing_heap.top heap with
      (* check if the next item on the heap is pointing at the same node, but
         comes from a different parent; if so => win *)
      | Some (j, _, q) when i = j && Bool.(p <> q) ->
          lca := Some i ;
          false
      (* otherwise, add all parents of the node to the heap and continue *)
      | _ ->
          List.iter g.supported_by.(i) ~f:(fun k ->
              Pairing_heap.add heap (k, g.zpos.(k), p) ) ;
          true )
  do
    ()
  done ;
  !lca

let rec lca g nodes =
  (* lowest common ancestor of multiple nodes *)
  match nodes with
  | [] -> None
  | [x] -> Some x
  | hd :: tl -> lca g tl |> Option.bind ~f:(lca_pair g hd)

let print_dot g =
  printf "digraph g {\n" ;
  for i = 0 to Array.length g.supports - 1 do
    List.iter g.supports.(i) ~f:(fun j -> printf "  a%d -> a%d;\n" i j) ;
    if List.length g.supported_by.(i) > 1 then
      match lca g g.supported_by.(i) with
      | None -> printf "  a%d [color=red];\n" i
      | Some j -> printf "  a%d -> a%d [color=red];\n" i j
  done ;
  printf "}\n"

module M = struct
  type t = brick array

  let parse s =
    let module P = Imports.StreamParser in
    let p = P.create s in
    let bricks = ref [] in
    while P.not_eof p do
      let x0 = P.parse_digit_u p in
      P.skip p 1 ;
      let y0 = P.parse_digit_u p in
      P.skip p 1 ;
      let z0 = P.parse_int p in
      let x1 = P.parse_digit_u p in
      P.skip p 1 ;
      let y1 = P.parse_digit_u p in
      P.skip p 1 ;
      let z1 = P.parse_int p in
      assert (x1 >= x0 && y1 >= y0 && z1 >= z0) ;
      let len, rot =
        match (x0 <> x1, y0 <> y1, z0 <> z1) with
        | true, false, false | false, false, false -> (x1 - x0, 0)
        | false, true, false -> (y1 - y0, 1)
        | false, false, true -> (z1 - z0, 2)
        | _ -> assert false
      in
      let brick = {x= x0; y= y0; z= z0; len; rot} in
      bricks := brick :: !bricks
    done ;
    List.sort !bricks ~compare:(fun a b -> compare a.z b.z) |> Array.of_list

  let part1 bricks =
    (* 515 *)
    let g = find_supports bricks in
    Array.count g.supports
      ~f:(List.for_all ~f:(fun i -> List.length g.supported_by.(i) > 1))
    |> Int.to_string

  let part2 bricks =
    (* 101541 *)
    let n = Array.length bricks in
    let g = find_supports bricks in
    let filter_out l v = List.filter l ~f:(fun x -> x <> v) in
    let unlink_node i =
      List.iter g.supported_by.(i) ~f:(fun j ->
          g.supports.(j) <- filter_out g.supports.(j) i )
    in
    (* using LCA for multi-parent nodes, convert the DAG into a tree *)
    for i = 0 to n - 1 do
      if List.length g.supported_by.(i) > 1 then (
        let lca = lca g g.supported_by.(i) in
        unlink_node i ;
        match lca with
        | None -> g.supported_by.(i) <- []
        | Some j ->
            g.supported_by.(i) <- [j] ;
            g.supports.(j) <- i :: g.supports.(j) )
    done ;
    (* now that we have a tree, just walk from top down and count *)
    let idx_rev =
      Array.mapi g.zpos ~f:(fun i h -> (h, i))
      |> Array.sorted_copy ~compare:(fun (_, h1) (_, h2) -> compare h1 h2)
      |> Array.map ~f:snd
    in
    let n_will_fall = Array.create ~len:n 0 in
    Array.fold idx_rev ~init:0 ~f:(fun acc i ->
        let count =
          List.fold g.supported_by.(i) ~init:0 ~f:(fun acc j ->
              acc + 1 + n_will_fall.(j) )
        in
        n_will_fall.(i) <- count ;
        acc + count )
    |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "1,0,1~1,2,1\n\
   0,0,2~2,0,2\n\
   0,2,3~2,2,3\n\
   0,0,4~0,2,4\n\
   2,0,5~2,2,5\n\
   0,1,6~2,1,6\n\
   1,1,8~1,1,9" |> run_test ;
  [%expect {| 5 7 |}]

open Core

type map = {w: int; h: int; a: bytes}

let _MAX = 1 lsl 32

module Map = struct
  let of_string s =
    let n = String.length s in
    let w = String.index_exn s '\n' in
    let h = (n + 1) / (w + 1) in
    let k = (w + 2) * (h + 2) in
    let a = Bytes.create k in
    let i, j = (ref 0, ref (w + 3)) in
    Bytes.fill ~pos:0 ~len:(w + 3) a '\xff' ;
    while !i < n do
      match s.[!i] with
      | '\n' ->
          incr i ;
          Bytes.set a !j '\xff' ;
          incr j ;
          Bytes.set a !j '\xff' ;
          incr j
      | c ->
          Bytes.set a !j
            (Char.unsafe_of_int (Char.to_int c - Char.to_int '0')) ;
          incr i ;
          incr j
    done ;
    Bytes.fill ~pos:!j ~len:(k - !j) a '\xff' ;
    {w; h; a}

  let dump {w; h; a} =
    for i = 1 to h do
      for j = 1 to w do
        let c = Bytes.get a (((w + 2) * i) + j) in
        printf "%c" (Char.unsafe_of_int (Char.to_int '0' + Char.to_int c))
      done ;
      print_endline ""
    done ;
    for j = 1 to w do
      assert (Char.(Bytes.get a j = '\xff')) ;
      assert (Char.(Bytes.get a (j + ((h + 1) * (w + 2))) = '\xff'))
    done ;
    for i = 1 to h do
      assert (Char.(Bytes.get a ((w + 2) * i) = '\xff')) ;
      assert (Char.(Bytes.get a (((w + 2) * i) + w + 1) = '\xff'))
    done

  let dump_dist_map {w; h; _} d =
    for i = 1 to h do
      for j = 1 to w do
        let v = Array.get d (((w + 2) * i) + j) in
        let s = if v >= 0 && v <> _MAX then Int.to_string v else "." in
        printf "%4s" s
      done ;
      print_endline ""
    done

  let rev_dijkstra_no_turn_constraints {w; h; a} =
    let row = w + 2 in
    let n = Bytes.length a in
    let frontier =
      Pairing_heap.create ~min_size:n
        ~cmp:(fun (_, d1) (_, d2) -> Int.compare d1 d2)
        ()
    in
    let start = (h * row) + w in
    let dist = Array.create ~len:n _MAX in
    dist.(start) <- 0 ;
    Pairing_heap.add frontier (start, 0) ;
    let neighbor_offsets = [|1; -1; row; -row|] in
    let rec loop () =
      match Pairing_heap.pop frontier with
      | None -> ()
      | Some (u, d) ->
          for i = 0 to 3 do
            let next = u + neighbor_offsets.(i) in
            let cost = Bytes.get a next |> Char.to_int in
            (* cost of original because we travel backwards *)
            let cost_u = Bytes.get a u |> Char.to_int in
            let new_cost = d + cost_u in
            let next_dist = dist.(next) in
            (* TODO: can remove if = _MAX *)
            if cost <> 0xff && (next_dist = _MAX || new_cost < next_dist)
            then (
              dist.(next) <- new_cost ;
              Pairing_heap.add frontier (next, new_cost) )
          done ;
          loop ()
    in
    loop () ; dist

  let a_star_with_turn_constraints {w; h; a} d =
    (* we'll use dijkstra distance matrix as a* heuristic *)
    let module Local = struct
      type state =
        { pos: int (* position in the map *)
        ; dir: int (* direction last travelled, nesw=0123 *)
        ; count: int (* how many steps in this direction, minus 1 *)
        ; dist: int (* distance travelled so far *)
        ; prio: int (* heap priority *) }
    end in
    let open Local in
    let row = w + 2 in
    let n = Bytes.length a in
    let frontier =
      Pairing_heap.create ~min_size:n
        ~cmp:(fun s1 s2 -> Int.compare s1.prio s2.prio)
        ()
    in
    let start, finish = ((1 * row) + 1, (h * row) + w) in
    let get_index pos dir count = (pos * 12) + (dir * 3) + count in
    let dist = Array.create ~len:(n * 12) _MAX in
    for dir = 0 to 3 do
      for count = 0 to 2 do
        dist.(get_index start dir count) <- 0
      done
    done ;
    let start_state pos dir =
      let d = Bytes.get a pos |> Char.to_int in
      {pos; dir; count= 0; prio= d; dist= d}
    in
    Pairing_heap.add frontier (start_state (start + 1) 1) ;
    Pairing_heap.add frontier (start_state (start + row) 2) ;
    let neighbor_offsets = [|-row; 1; row; -1|] in
    let rec loop () =
      match Pairing_heap.pop frontier with
      | None -> None
      | Some s when s.pos = finish ->
          Some dist.(get_index s.pos s.dir s.count)
      | Some s ->
          let options = [((s.dir + 1) land 3, 0); ((s.dir + 3) land 3, 0)] in
          let options =
            if s.count < 2 then (s.dir, s.count + 1) :: options else options
          in
          List.iter options ~f:(fun (dir, count) ->
              let next = s.pos + neighbor_offsets.(dir) in
              let cost = Bytes.get a next |> Char.to_int in
              let new_cost = s.dist + cost in
              let idx = get_index next dir count in
              let next_dist = dist.(idx) in
              if cost <> 0xff && new_cost < next_dist then (
                dist.(idx) <- new_cost ;
                let prio = new_cost + Array.get d next in
                Pairing_heap.add frontier
                  {pos= next; dir; count; dist= new_cost; prio} ) ) ;
          loop ()
    in
    loop () |> Option.value_exn

  let a_star_with_turn_constraints_ultra {w; h; a} d =
    (* we'll use dijkstra distance matrix as a* heuristic *)
    let module Local = struct
      type state =
        { pos: int (* position in the map *)
        ; x: int (* x position in the map *)
        ; y: int (* y position in the map *)
        ; dir: int (* direction last travelled, nesw=0123 *)
        ; count: int (* how many steps in this direction, minus 1 *)
        ; dist: int (* distance travelled so far *)
        ; prio: int (* heap priority *) }
    end in
    let open Local in
    let row = w + 2 in
    let n = Bytes.length a in
    let frontier =
      Pairing_heap.create ~min_size:n
        ~cmp:(fun s1 s2 -> Int.compare s1.prio s2.prio)
        ()
    in
    let start, finish = ((1 * row) + 1, (h * row) + w) in
    let get_index pos dir count = (pos * 24) + (dir * 6) + count in
    let dist = Array.create ~len:(n * 24) _MAX in
    for dir = 0 to 3 do
      for count = 0 to 5 do
        dist.(get_index start dir count) <- 0
      done
    done ;
    let start_state dir =
      (* count = 5 -> force to turn immediately *)
      dist.(get_index start dir 5) <- 0 ;
      {pos= start; x= 1; y= 1; dir; count= 5; prio= 0; dist= 0}
    in
    Pairing_heap.add frontier (start_state 1) ;
    Pairing_heap.add frontier (start_state 2) ;
    let neighbor_offsets =
      [|(-row, 0, -1); (1, 1, 0); (row, 0, 1); (-1, -1, 0)|]
    in
    let agg_costs =
      Array.init 4 ~f:(fun dir ->
          let arr = Bytes.create n in
          Bytes.fill arr ~pos:0 ~len:n '\x00' ;
          let dpos, _, _ = neighbor_offsets.(dir) in
          for y = 1 to h do
            for x = 1 to w do
              let p = (y * row) + x in
              let pos, v = (ref p, ref 0) in
              for _ = 1 to 4 do
                v :=
                  !v + Char.to_int (Bytes.get a (max 0 (min (n - 1) !pos))) ;
                pos := !pos - dpos
              done ;
              Bytes.set arr p (Char.unsafe_of_int !v)
            done
          done ;
          arr )
    in
    let rec loop () =
      match Pairing_heap.pop frontier with
      | None -> None
      | Some s when s.pos = finish -> Some s.dist
      | Some s ->
          for dir = 0 to 3 do
            let dpos, dx, dy = neighbor_offsets.(dir) in
            let steps = ref 0 in
            if dir = s.dir && s.count < 5 (* move forward *) then steps := 1
            else if dir land 1 <> s.dir land 1 (* turn left or right *) then
              steps := 4 ;
            match !steps with
            | 0 -> ()
            | steps ->
                let x, y = (s.x + (dx * steps), s.y + (dy * steps)) in
                if x >= 1 && x <= w && y >= 1 && y <= h then
                  (* important: count = -1 here and not 0 *)
                  let count = if steps = 1 then s.count + 1 else -1 in
                  let next = s.pos + (dpos * steps) in
                  let cost =
                    Bytes.get (if steps = 1 then a else agg_costs.(dir)) next
                  in
                  let new_cost = s.dist + Char.to_int cost in
                  let idx = get_index next dir count in
                  let next_dist = dist.(idx) in
                  if new_cost < next_dist then (
                    dist.(idx) <- new_cost ;
                    let prio = new_cost + Array.get d next in
                    Pairing_heap.add frontier
                      {pos= next; x; y; dir; count; dist= new_cost; prio} )
          done ;
          loop ()
    in
    loop () |> Option.value_exn
end

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 1138 *)
    let map = Map.of_string s in
    let dist = Map.rev_dijkstra_no_turn_constraints map in
    Map.a_star_with_turn_constraints map dist |> Int.to_string

  let part2 s =
    (* 1312 *)
    let map = Map.of_string s in
    let dist = Map.rev_dijkstra_no_turn_constraints map in
    Map.a_star_with_turn_constraints_ultra map dist |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "2413432311323\n\
   3215453535623\n\
   3255245654254\n\
   3446585845452\n\
   4546657867536\n\
   1438598798454\n\
   4457876987766\n\
   3637877979653\n\
   4654967986887\n\
   4564679986453\n\
   1224686865563\n\
   2546548887735\n\
   4322674655533" |> run_test ;
  [%expect {| 102 94 |}]

let%expect_test _ =
  "111111111111\n999999999991\n999999999991\n999999999991\n999999999991"
  |> run_test ~part:2 ;
  [%expect {| 71 |}]

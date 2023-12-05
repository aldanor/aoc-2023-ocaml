open Imports
open Core

module Interval = struct
  type t = {low: int; high: int} [@@deriving show]

  let create low high = {low; high}

  let contains {low; high} value = low <= value && value < high

  let overlaps a b = max a.low b.low < min a.high b.high

  let is_empty {low; high} = low >= high

  let shift {low; high} value = {low= low + value; high= high + value}

  let compare_low a b = compare a.low b.low

  let merge a b =
    if max a.low b.low > min a.high b.high then None
    else
      let low, high = (min a.low b.low, max a.high b.high) in
      Some {low; high}

  let intersect a b =
    let low, high = (max a.low b.low, min a.high b.high) in
    if low < high then Some {low; high} else None

  (* returns collection of intervals that join up to `a`, with a flag of
     whether it's an intersection with `b` or not *)
  let split_by_interval a b =
    let low, high = (max a.low b.low, min a.high b.high) in
    let center = {low; high} in
    if is_empty center then [(a, false)]
    else
      let left = {low= a.low; high= low} in
      let right = {low= high; high= a.high} in
      let left_right =
        match (is_empty left, is_empty right) with
        | true, true -> []
        | true, false -> [(right, false)]
        | false, true -> [(left, false)]
        | false, false -> [(left, false); (right, false)]
      in
      (center, true) :: left_right
end

module IntervalTree = struct
  type 'a split = {left: 'a tree; right: 'a tree} [@@deriving show]

  and 'a node = Leaf of 'a | Split of 'a split [@@deriving show]

  and 'a tree = {interval: Interval.t; node: 'a node} [@@deriving show]

  (* for the sake of this task, we assume that intervals are non-empty and
     non-overlapping *)
  let of_assoc (items : (Interval.t * 'a) list) =
    let rec build_tree = function
      | [] -> failwith "empty list"
      | [(i, v)] -> {interval= i; node= Leaf v}
      | items ->
          let n = List.length items in
          let items_l, items_r = List.split_n items (n / 2) in
          let left, right = (build_tree items_l, build_tree items_r) in
          let interval =
            Interval.create left.interval.low right.interval.high
          in
          {interval; node= Split {left; right}}
    in
    let compare = Interval.(fun (a, _) (b, _) -> Int.compare a.low b.low) in
    items |> List.sort ~compare |> build_tree

  let traverse tree value =
    let rec traverse' {interval; node} =
      match node with
      | Leaf v -> if Interval.contains interval value then Some v else None
      | Split {left; right} ->
          if value < left.interval.high then traverse' left
          else if value >= right.interval.low then traverse' right
          else None
    in
    traverse' tree

  (* split input interval into sub-intervals paired with matching leaf values
     in the interval tree (or default if there's no overlap); the output
     intervals are guaranteed to add up exactly to the input interval *)
  let map_interval ?(default = 0) tree interval =
    let rec map_interval' {interval; node} acc i =
      match node with
      | Leaf v ->
          List.iter (Interval.split_by_interval i interval)
            ~f:(fun (i, overlaps) ->
              let v = if overlaps then v else default in
              acc := (i, v) :: !acc )
      | Split {left; right} ->
          let split = right.interval.low in
          if i.low < split then
            map_interval' left acc {i with high= min i.high split} ;
          if i.high > split then
            map_interval' right acc {i with low= max i.low split}
    in
    let acc = ref [] in
    map_interval' tree acc interval ;
    !acc
end

let parse_input s =
  let module P = StreamParser in
  let p = P.create s in
  P.skip p 7 ;
  let seeds = ref [] in
  while P.not_whitespace p do
    seeds := P.parse_int p :: !seeds
  done ;
  let trees = ref [] in
  while P.not_eof p do
    let _ = P.skip p 1 ; P.skip_to p '\n' ; P.skip p 1 in
    let block = ref [] in
    while P.not_whitespace p do
      let dst, src, len = P.parse_int3 p in
      let i = Interval.create src (src + len) in
      let v = dst - src in
      block := (i, v) :: !block
    done ;
    let tree = IntervalTree.of_assoc !block in
    trees := tree :: !trees
  done ;
  (List.rev !seeds, List.rev !trees)

let rec seeds_to_intervals = function
  | low :: len :: tl ->
      Interval.create low (low + len) :: seeds_to_intervals tl
  | _ -> []

module M = struct
  type t = int list * int IntervalTree.tree list

  let parse = parse_input

  let part1 (seeds, trees) =
    (* 227653707 *)
    List.map seeds ~f:(fun seed ->
        List.fold trees ~init:seed ~f:(fun loc tree ->
            loc
            + (loc |> IntervalTree.traverse tree |> Option.value ~default:0) ) )
    |> List.min_elt ~compare |> Option.value ~default:0 |> Int.to_string

  let part2 (seeds, trees) =
    (* 78775051 *)
    let sort_intervals = List.sort ~compare:Interval.compare_low in
    let compare_iv (a, _) (b, _) = Interval.compare_low a b in
    let sort_ivs = List.sort ~compare:compare_iv in
    let apply_ivs = List.map ~f:(fun (i, v) -> Interval.shift i v) in
    let rec merge_sorted = function
      | a :: b :: tl -> (
        match Interval.merge a b with
        | Some i -> merge_sorted (i :: tl)
        | None -> a :: merge_sorted (b :: tl) )
      | x -> x
    in
    let pass_through_tree intervals tree =
      intervals
      |> List.map ~f:(IntervalTree.map_interval tree)
      |> List.concat |> sort_ivs |> apply_ivs |> sort_intervals
      |> merge_sorted
    in
    let intervals = seeds_to_intervals seeds in
    trees
    |> List.fold ~init:intervals ~f:pass_through_tree
    |> List.min_elt ~compare:Interval.compare_low
    |> Option.value_map ~default:(-1) ~f:(fun i -> i.Interval.low)
    |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "seeds: 79 14 55 13\n\n\
   seed-to-soil map:\n\
   50 98 2\n\
   52 50 48\n\n\
   soil-to-fertilizer map:\n\
   0 15 37\n\
   37 52 2\n\
   39 0 15\n\n\
   fertilizer-to-water map:\n\
   49 53 8\n\
   0 11 42\n\
   42 0 7\n\
   57 7 4\n\n\
   water-to-light map:\n\
   88 18 7\n\
   18 25 70\n\n\
   light-to-temperature map:\n\
   45 77 23\n\
   81 45 19\n\
   68 64 13\n\n\
   temperature-to-humidity map:\n\
   0 69 1\n\
   1 0 69\n\n\
   humidity-to-location map:\n\
   60 56 37\n\
   56 93 4" |> run_test ;
  [%expect {| 35 46 |}]

open Core

module Interval = struct
  (* could make it generic over the comparable type etc, but we won't
     over-generalize it for this task *)
  type t = {low: int; high: int} [@@deriving show]

  let create low high = {low; high}

  let contains {low; high} value = low <= value && value < high
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
end

module StreamParser = struct
  type t = {s: string; n: int; mutable pos: int}

  let create s = {s; n= String.length s; pos= 0}

  let parse_int ?(skip = 1) p =
    let rec parse_int' acc =
      if p.pos = p.n then acc
      else
        let c = String.get p.s p.pos in
        match c with
        | '0' .. '9' as c ->
            p.pos <- p.pos + 1 ;
            parse_int' ((acc * 10) + Char.to_int c - 48)
        | _ -> acc
    in
    let ans = parse_int' 0 in
    p.pos <- p.pos + skip ;
    ans

  let parse_int2 ?skip p =
    let a = parse_int ?skip p in
    let b = parse_int ?skip p in
    (a, b)

  let parse_int3 ?skip p =
    let a = parse_int ?skip p in
    let b = parse_int ?skip p in
    let c = parse_int ?skip p in
    (a, b, c)

  let pos p = p.pos

  let skip p n = p.pos <- p.pos + n

  let is_eof p = p.pos >= p.n

  let not_eof p = p.pos < p.n

  let is_whitespace_u p =
    let c = String.unsafe_get p.s p.pos in
    Char.(c = ' ' || c = '\n')

  let not_whitespace p = not_eof p && not (is_whitespace_u p)

  let hd_u p = String.unsafe_get p.s p.pos

  let hd_equals_u p c = Char.(String.unsafe_get p.s p.pos = c)

  let skip_to p c =
    while (not (is_eof p)) && not (hd_equals_u p c) do
      skip p 1
    done
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
    (* printf "%s\n" (IntervalTree.show_tree Format.pp_print_int tree) ; *)
    trees := tree :: !trees
  done ;
  (* seeds will be reversed but it's ok; blocks order matters though *)
  (!seeds, List.rev !trees)

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

  let part2 (_seeds, _trees) = ""
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
  [%expect {| 35 |}]

open Core
module P = Imports.StreamParser

type direction = Left | Right | Up | Down
[@@deriving show {with_path= false}]

type color = {r: int; g: int; b: int} [@@deriving show {with_path= false}]

type instruction = {d: direction; n: int; c: color}
[@@deriving show {with_path= false}]

let _N = 1024

type pos = {x: int; y: int} [@@deriving show {with_path= false}]

let pos_to_idx {x; y} = (y * _N) + x

let dir_to_idx_offset = function
  | Left -> -1
  | Right -> 1
  | Up -> -_N
  | Down -> _N

let apply_instruction {x; y} {d; n; _} =
  let x, y =
    match d with
    | Left -> (x - n, y)
    | Right -> (x + n, y)
    | Up -> (x, y - n)
    | Down -> (x, y + n)
  in
  {x; y}

let parse_instruction (p : P.t) : instruction option =
  let parse_direction () =
    match P.parse_char_u p with
    | 'L' -> Left
    | 'R' -> Right
    | 'U' -> Up
    | _ -> Down
  in
  let parse_hex_digit () =
    match P.parse_char_u p with
    | '0' .. '9' as c -> Char.to_int c - Char.to_int '0'
    | c -> Char.to_int c - Char.to_int 'a' + 10
  in
  let parse_hex () =
    let hi = parse_hex_digit () in
    let lo = parse_hex_digit () in
    (hi * 16) + lo
  in
  let parse_color () =
    let r = parse_hex () in
    let g = parse_hex () in
    let b = parse_hex () in
    {r; g; b}
  in
  if P.is_eof p then None
  else
    Some
      (let d = parse_direction () in
       P.skip p 1 ;
       let n = P.parse_int p in
       P.skip p 2 ;
       let c = parse_color () in
       P.skip p 2 ; {d; n; c} )

let parse_instructions (s : string) : instruction list =
  let p = P.create s in
  let rec loop acc =
    match parse_instruction p with
    | None -> List.rev acc
    | Some i -> loop (i :: acc)
  in
  loop []

let find_start_pos instructions =
  (* absolute offset of the first point *)
  let fold (pos, lo, hi) i =
    let pos = apply_instruction pos i in
    let lo = {x= min lo.x pos.x; y= min lo.y pos.y} in
    let hi = {x= max hi.x pos.x; y= max hi.y pos.y} in
    (pos, lo, hi)
  in
  let z = {x= 0; y= 0} in
  let {x; y}, {x= xmin; y= ymin}, {x= xmax; y= ymax} =
    List.fold instructions ~init:(z, z, z) ~f:fold
  in
  assert (x = 0 && y = 0) ;
  let w = max (xmax - xmin) (ymax - ymin) in
  assert (w + 2 <= _N) ;
  {x= -xmin + 1; y= -ymin + 1}

let fill_board instructions start =
  let board = Bytes.create (_N * _N) in
  Bytes.fill ~pos:0 ~len:(Bytes.length board) board '\x00' ;
  let rec loop pos instructions =
    match instructions with
    | [] -> ()
    | i :: is ->
        let idx = ref (pos_to_idx pos) in
        let offset = dir_to_idx_offset i.d in
        for _ = 1 to i.n do
          Bytes.set board !idx '\x01' ;
          idx := !idx + offset
        done ;
        loop (apply_instruction pos i) is
  in
  loop start instructions ; board

let print_board board =
  let (xmax, xmin), (ymax, ymin) =
    ((ref 0, ref (_N - 1)), (ref 0, ref (_N - 1)))
  in
  for y = 0 to _N - 1 do
    for x = 0 to _N - 1 do
      if Char.(Bytes.get board (pos_to_idx {x; y}) = '\x01') then (
        xmin := min !xmin x ;
        xmax := max !xmax x ;
        ymin := min !ymin y ;
        ymax := max !ymax y )
    done
  done ;
  for y = !ymin to !ymax do
    for x = !xmin to !xmax do
      let c = Bytes.get board (pos_to_idx {x; y}) in
      printf "%c" (match c with '\x01' -> '#' | '\x02' -> '+' | _ -> '.')
    done ;
    print_endline ""
  done

let fill_interior board =
  let rec find_start_at_row board y =
    find_start board {x= 1; y} (pos_to_idx {x= 1; y}) false
  and find_start board {x; y} idx seen_border =
    if x >= _N then find_start_at_row board (y + 1)
    else if y >= _N then assert false
    else
      match (Bytes.get board idx, seen_border) with
      | '\x01', false -> find_start board {x= x + 1; y} (idx + 1) true
      | '\x01', true -> find_start_at_row board (y + 1)
      | _, false -> find_start board {x= x + 1; y} (idx + 1) seen_border
      | _, true -> {x; y}
  in
  let start = find_start_at_row board 1 in
  let rec flash_fill queue count =
    match queue with
    | [] -> count
    | idx :: queue ->
        let c = Bytes.get board idx in
        if Char.(c = '\x00') then (
          Bytes.set board idx '\x02' ;
          flash_fill
            ((idx - 1) :: (idx + 1) :: (idx - _N) :: (idx + _N) :: queue)
            (count + 1) )
        else flash_fill queue count
  in
  flash_fill [pos_to_idx start] 0

let compute_perimeter instructions =
  List.fold ~init:0 ~f:(fun acc i -> acc + i.n) instructions

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 47527 *)
    let instructions = parse_instructions s in
    let start = find_start_pos instructions in
    let board = fill_board instructions start in
    let perimeter = compute_perimeter instructions in
    let interior = fill_interior board in
    interior + perimeter |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "R 6 (#70c710)\n\
   D 5 (#0dc571)\n\
   L 2 (#5713f0)\n\
   D 2 (#d2c081)\n\
   R 2 (#59c680)\n\
   D 2 (#411b91)\n\
   L 5 (#8ceee2)\n\
   U 2 (#caa173)\n\
   L 1 (#1b58a2)\n\
   U 2 (#caa171)\n\
   R 2 (#7807d2)\n\
   U 3 (#a77fa3)\n\
   L 2 (#015232)\n\
   U 2 (#7a21e3)" |> run_test ;
  [%expect {| 62 |}]

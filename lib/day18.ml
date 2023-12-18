open Core
module P = Imports.StreamParser

type direction = Left | Right | Up | Down
[@@deriving show {with_path= false}]

type instruction = {d: direction; n: int}
[@@deriving show {with_path= false}]

type pos = {x: int; y: int} [@@deriving show {with_path= false}]

let apply_instruction {x; y} {d; n} =
  let x, y =
    match d with
    | Left -> (x - n, y)
    | Right -> (x + n, y)
    | Up -> (x, y - n)
    | Down -> (x, y + n)
  in
  {x; y}

let parse_instruction_1 (p : P.t) : instruction option =
  let parse_dir () =
    match P.parse_char_u p with
    | 'L' -> Left
    | 'R' -> Right
    | 'U' -> Up
    | _ -> Down
  in
  if P.is_eof p then None
  else
    Some
      (let d = parse_dir () in
       P.skip p 1 ;
       let n = P.parse_int p in
       P.skip p 10 ; {d; n} )

let parse_instruction_2 (p : P.t) : instruction option =
  let parse_hex () =
    match P.parse_char_u p with
    | '0' .. '9' as c -> Char.to_int c - Char.to_int '0'
    | c -> Char.to_int c - Char.to_int 'a' + 10
  in
  let parse_len () =
    let d4 = parse_hex () in
    let d3 = parse_hex () in
    let d2 = parse_hex () in
    let d1 = parse_hex () in
    let d0 = parse_hex () in
    (d4 lsl 16) lor (d3 lsl 12) lor (d2 lsl 8) lor (d1 lsl 4) lor d0
  in
  let parse_dir () =
    match P.parse_char_u p with
    | '0' -> Right
    | '1' -> Down
    | '2' -> Left
    | _ -> Up
  in
  if P.is_eof p then None
  else
    Some
      ( P.skip p 6 ;
        if P.hd_equals_u p '#' then P.skip p 1 ;
        let n = parse_len () in
        let d = parse_dir () in
        P.skip p 2 ; {d; n} )

let solve part s =
  let parser = P.create s in
  let parse () =
    if part = 1 then parse_instruction_1 parser
    else parse_instruction_2 parser
  in
  let rec f prev a p =
    match parse () with
    | None -> ((abs a + p) / 2) + 1
    | Some i ->
        let next = apply_instruction prev i in
        let a = a + ((prev.x * next.y) - (prev.y * next.x)) in
        let p = p + i.n in
        f next a p
  in
  f {x= 0; y= 0} 0 0

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 47527 *)
    s |> solve 1 |> Int.to_string

  let part2 s =
    (* 52240187443190 *)
    s |> solve 2 |> Int.to_string
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
  [%expect {| 62 952408144115 |}]

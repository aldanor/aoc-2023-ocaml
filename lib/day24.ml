open Core

let pp = Bigint.pp

module B = Bigint

type big_int = Bigint.t

type point = {x: big_int; y: big_int; z: big_int}
(* [@@deriving eq] *)

let pp_point ppf {x; y; z} =
  (* Format.fprintf ppf "(%d, %d, %d)" x y z *)
  Format.fprintf ppf "(%a, %a, %a)" pp x pp y pp z

let show_point {x; y; z} =
  (* sprintf "(%d, %d, %d)" x y z *)
  Format.asprintf "(%a, %a, %a)" pp x pp y pp z

type hailstone = {p: point; v: point}

let pp_hailstone ppf {p; v} =
  (* Format.fprintf ppf "%d, %d, %d @ %d, %d, %d" p.x p.y p.z v.x v.y v.z *)
  Format.fprintf ppf "%a, %a, %a : %a, %a, %a" pp p.x pp p.y pp p.z pp v.x pp
    v.y pp v.z

let show_hailstone {p; v} =
  (* sprintf "%d, %d, %d @ %d, %d, %d" p.x p.y p.z v.x v.y v.z *)
  Format.asprintf "%a, %a, %a : %a, %a, %a" pp p.x pp p.y pp p.z pp v.x pp
    v.y pp v.z

type line = {p1: point; p2: point} [@@deriving show {with_path= false}]

type box = {min: point; max: point} [@@deriving show {with_path= false}]

let bounds ~test =
  let lo, hi =
    if test then (7, 27) else (200000000000000, 400000000000000)
  in
  (Bigint.of_int lo, Bigint.of_int hi)

module M = struct
  type t = hailstone list

  let parse s =
    let module P = Imports.StreamParser in
    let p = P.create s in
    let out = ref [] in
    while P.not_eof p do
      let x = P.parse_int p ~skip:2 |> B.of_int in
      let y = P.parse_int p ~skip:2 |> B.of_int in
      let z = P.parse_int p ~skip:3 |> B.of_int in
      P.skip_whitespace p ;
      let vx = P.parse_signed_int p ~skip:2 |> B.of_int in
      P.skip_whitespace p ;
      let vy = P.parse_signed_int p ~skip:2 |> B.of_int in
      P.skip_whitespace p ;
      let vz = P.parse_signed_int p ~skip:1 |> B.of_int in
      out := {p= {x; y; z}; v= {x= vx; y= vy; z= vz}} :: !out
    done ;
    !out

  let part1 ps =
    (* 16050 *)
    let ps = Array.of_list (List.rev ps) in
    let n = Array.length ps in
    let lo, hi = bounds ~test:(n < 10) in
    let intersection_in_bounds h1 h2 =
      let open Bigint.O in
      let det a b c d = (a * d) - (b * c) in
      let a1, b1 = (h1.v.y, -h1.v.x) in
      let c1 = det (h1.p.x + h1.v.x) (h1.p.y + h1.v.y) h1.p.x h1.p.y in
      let a2, b2 = (h2.v.y, -h2.v.x) in
      let c2 = det (h2.p.x + h2.v.x) (h2.p.y + h2.v.y) h2.p.x h2.p.y in
      let d = det a1 b1 a2 b2 in
      if d = B.zero then (* parallel *)
        false
      else
        let xi = det (-c1) b1 (-c2) b2 in
        let yi = det a1 (-c1) a2 (-c2) in
        let lo, hi =
          if B.is_non_negative d then (lo * d, hi * d) else (hi * d, lo * d)
        in
        let in_bounds = xi >= lo && xi <= hi && yi >= lo && yi <= hi in
        if in_bounds then
          (* within box bounds *)
          let dp v1 v2 = (v1.x * v2.x) + (v1.y * v2.y) in
          let i1x, i1y = (xi - (d * h1.p.x), yi - (d * h1.p.y)) in
          let i2x, i2y = (xi - (d * h2.p.x), yi - (d * h2.p.y)) in
          let i1, i2 =
            if B.is_non_negative d then
              ({x= i1x; y= i1y; z= B.zero}, {x= i2x; y= i2y; z= B.zero})
            else
              ( {x= B.neg i1x; y= B.neg i1y; z= B.zero}
              , {x= B.neg i2x; y= B.neg i2y; z= B.zero} )
          in
          let d1, d2 = (dp i1 h1.v, dp i2 h2.v) in
          B.is_non_negative d1 && B.is_non_negative d2
        else (* outside of box bounds *)
          false
    in
    let count = ref 0 in
    for i = 0 to n - 2 do
      for j = i + 1 to n - 1 do
        count := !count + Bool.to_int (intersection_in_bounds ps.(i) ps.(j))
      done
    done ;
    !count |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "19, 13, 30 @ -2,  1, -2\n\
   18, 19, 22 @ -1, -1, -2\n\
   20, 25, 34 @ -2, -2, -4\n\
   12, 31, 28 @ -1, -2, -1\n\
   20, 19, 15 @  1, -5, -3" |> run_test ;
  [%expect {| 2 |}]

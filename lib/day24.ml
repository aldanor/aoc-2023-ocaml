open Core

let pp = Bigint.pp

module B = Bigint

type big_int = Bigint.t

type num_array = Bignum.t array

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

module Gaussian = struct
  module Array = struct
    include Array

    (* Computes: f a.(0) + f a.(1) + ... where + is 'g'. *)
    let foldmap g f a =
      let n = Array.length a in
      let rec aux acc i =
        if i >= n then acc else aux (g acc (f a.(i))) (succ i)
      in
      aux (f a.(0)) 1
  end

  let foldmap_range g f (a, b) =
    let rec aux acc n =
      let n = succ n in
      if n > b then acc else aux (g acc (f n)) n
    in
    aux (f a) a

  let fold_range f init (a, b) =
    let rec aux acc n = if n > b then acc else aux (f acc n) (succ n) in
    aux init a

  (* Solve Ax=b for x, using gaussian elimination with scaled partial pivot,
   * and then back-substitution of the resulting row-echelon matrix. *)
  let solve (m : num_array array) (b : num_array) : num_array =
    let open Bignum.O in
    let n = Array.length m in
    let n' = pred n in
    let s = Array.(map ~f:(foldmap Bignum.max Bignum.abs) m) in
    (* scaling vector *)
    let a = Array.(init n ~f:(fun i -> append m.(i) [|b.(i)|])) in
    for k = 0 to pred n' do
      (* scaled partial pivot, to preserve precision *)
      let pair i = (i, Bignum.abs (a.(i).(k) / s.(i))) in
      let maxsnd a b = if snd a > snd b then a else b in
      let i_max, v = foldmap_range maxsnd pair (k, n') in
      if v < Bignum.trillionth then failwith "matrix is near-singular" ;
      Array.swap a k i_max ;
      Array.swap s k i_max ;
      (* eliminate one column *)
      for i = succ k to n' do
        let tmp = a.(i).(k) / a.(k).(k) in
        for j = succ k to n do
          a.(i).(j) <- a.(i).(j) - (tmp * a.(k).(j))
        done
      done
    done ;
    (* backward substitution; 'b' is in the 'nth' column of 'a' *)
    let x = Array.copy b in
    for i = n' downto 0 do
      let minus_dprod t j = t - (x.(j) * a.(i).(j)) in
      x.(i) <- fold_range minus_dprod a.(i).(n) (succ i, n') / a.(i).(i)
    done ;
    x
end

module M = struct
  type t = hailstone array

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
    !out |> List.rev |> Array.of_list

  let part1 ps =
    (* 16050 *)
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

  let part2 ps =
    (* 669042940632377 *)
    ()
    (*
    For a given i, we have
    Pi + ti * Vi = P' + ti * V'
    Pi - P' = ti * (V' - Vi)
    => P_i - P' and V' - V_i are parallel, so
    (V' - V_i) x (P_i - P') = 0
    Expanding the terms, we get
    P_i x V' - P_i x V_i - P' x V' + P' x V_i = 0
    P_i x V' - P_i x V_i + P' x V_i = P' x V'
    Right-hand side will be identical for all equations.
    Consider a pair (i, j), we can then eliminate RHS non-linear term:
    (vy' - vy_i) * (z_i - z') - (vz' - vz_i) * (y_i - y')
      = (vy' - vy_j) * (z_j - z') - (vz' - vz_j) * (y_j - y')
    (vz' - vz_i) * (x_i - x') - (vx' - vx_i) * (z_i - z')
      = (vz' - vz_j) * (x_j - x') - (vx' - vx_j) * (z_j - z')
    (vx' - vx_i) * (y_i - y') - (vy' - vy_i) * (x_i - x') 
      = (vx' - vx_j) * (y_j - y') - (vy' - vy_j) * (x_j - x')
    *)
    [@ocamlformat "wrap-comments=false"] ;
    let eqn_6x3_int hi hj =
      let open Bigint.O in
      let xi, yi, zi = (hi.p.x, hi.p.y, hi.p.z) in
      let xj, yj, zj = (hj.p.x, hj.p.y, hj.p.z) in
      let vxi, vyi, vzi = (hi.v.x, hi.v.y, hi.v.z) in
      let vxj, vyj, vzj = (hj.v.x, hj.v.y, hj.v.z) in
      let dx, dy, dz = (xi - xj, yi - yj, zi - zj) in
      let dvx, dvy, dvz = (vxi - vxj, vyi - vyj, vzi - vzj) in
      let yzi, yzj = ((vyi * zi) - (vzi * yi), (vyj * zj) - (vzj * yj)) in
      let zxi, zxj = ((vzi * xi) - (vxi * zi), (vzj * xj) - (vxj * zj)) in
      let xyi, xyj = ((vxi * yi) - (vyi * xi), (vxj * yj) - (vyj * xj)) in
      let rx, ry, rz = (-yzi + yzj, -zxi + zxj, -xyi + xyj) in
      let m =
        [| [|B.zero; dvz; -dvy; B.zero; dz; -dy|]
         ; [|-dvz; B.zero; dvx; -dz; B.zero; dx|]
         ; [|dvy; -dvx; B.zero; dy; -dx; B.zero|] |]
      in
      let b = [|rx; ry; rz|] in
      (m, b)
    in
    let eqn_6x6_num hi hj hk =
      let mij, bij = eqn_6x3_int hi hj in
      let mjk, bjk = eqn_6x3_int hj hk in
      let conv_1d = Array.map ~f:Bignum.of_bigint in
      let conv_2d = Array.map ~f:conv_1d in
      let m = Array.concat [mij; mjk] |> conv_2d in
      let b = Array.concat [bij; bjk] |> conv_1d in
      (m, b)
    in
    let m, b = eqn_6x6_num ps.(0) ps.(1) ps.(2) in
    let s = Gaussian.solve m b in
    Bignum.O.(s.(0) + s.(1) + s.(2))
    |> Bignum.to_bigint_opt |> Option.value_exn |> Bigint.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "19, 13, 30 @ -2,  1, -2\n\
   18, 19, 22 @ -1, -1, -2\n\
   20, 25, 34 @ -2, -2, -4\n\
   12, 31, 28 @ -1, -2, -1\n\
   20, 19, 15 @  1, -5, -3" |> run_test ;
  [%expect {| 2 47 |}]

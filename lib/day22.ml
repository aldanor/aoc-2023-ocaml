open Core

(* actual length is len + 1; rot = x=0, y=1, z=2 *)
type brick = {x: int; y: int; z: int; len: int; rot: int}
[@@deriving show {with_path= false}]

module M = struct
  type t = brick list

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
    List.sort !bricks ~compare:(fun a b -> compare a.z b.z)

  let part1 bricks =
    (* 515 *)
    let heights = Array.create ~len:121 0 in
    let top_bricks = Array.create ~len:121 (-1) in
    let supports = Array.init (List.length bricks) ~f:(fun _ -> []) in
    let supported_by = Array.init (List.length bricks) ~f:(fun _ -> []) in
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
      for j = 0 to hlen do
        h := max !h heights.(!k) ;
        k := !k + offset
      done ;
      let h, k = (!h, ref pos) in
      for j = 0 to hlen do
        ( if heights.(!k) = h && h <> 0 then
            let t = top_bricks.(!k) in
            match List.hd supports.(t) with
            | Some x when x = i -> ()
            | _ ->
                supports.(t) <- i :: supports.(t) ;
                supported_by.(i) <- t :: supported_by.(i) ) ;
        heights.(!k) <- h + dh ;
        top_bricks.(!k) <- i ;
        k := !k + offset
      done
    in
    List.iteri bricks ~f:process_brick ;
    Array.count supports
      ~f:(List.for_all ~f:(fun i -> List.length supported_by.(i) > 1))
    |> Int.to_string

  let part2 _ = ""
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
  [%expect {| 5 |}]

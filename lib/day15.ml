open Core

let hash_char h c =
  (* ocaml is apparently not smart enough to optimize all this *)
  let h = h + Char.to_int c in
  (h + (h lsl 4)) land 0xff

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    let f (sum, hash) c =
      match c with ',' -> (sum + hash, 0) | c -> (sum, hash_char hash c)
    in
    String.fold s ~init:(0, 0) ~f |> (fun (a, b) -> a + b) |> Int.to_string

  let part2 s =
    (* 229271 *)
    let module P = Imports.StreamParser in
    let boxes = Array.init 256 ~f:(fun _ -> []) in
    let p = P.create s in
    let rec remove_box box k =
      match box with
      | [] -> []
      | (k', v') :: tl -> if k = k' then tl else (k', v') :: remove_box tl k
    in
    let rec add_box box k v =
      (* List.Assoc.add doesn't retain order which we need *)
      match box with
      | [] -> [(k, v)]
      | (k', v') :: tl ->
          if k = k' then (k', v) :: tl else (k', v') :: add_box tl k v
    in
    while P.not_eof p do
      let h, i = (ref 0, ref 0) in
      while
        match P.parse_char_u p with
        | '-' ->
            let h = !h in
            Array.unsafe_set boxes h
            @@ remove_box (Array.unsafe_get boxes h) !i ;
            false
        | '=' ->
            let h, n = (!h, P.parse_digit_u p) in
            Array.unsafe_set boxes h
            @@ add_box (Array.unsafe_get boxes h) !i n ;
            false
        | c ->
            h := hash_char !h c ;
            i := (!i lsl 8) lor Char.to_int c ;
            true
      do
        ()
      done ;
      P.skip p 1
    done ;
    Array.foldi boxes ~init:0 ~f:(fun i acc box ->
        acc
        + List.foldi box ~init:0 ~f:(fun j acc' (_, n) ->
              acc' + ((i + 1) * (j + 1) * n) ) )
    |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "HASH" |> run_test ~part:1 ;
  [%expect {| 52 |}]

let%expect_test _ =
  "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" |> run_test ;
  [%expect {| 1320 145 |}]

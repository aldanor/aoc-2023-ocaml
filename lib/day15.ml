open Core

let hash_char h c = (h + Char.to_int c) * 17 mod 256

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
    let equal = String.equal in
    let rec remove_box box k ~equal =
      match box with
      | [] -> []
      | (k', v') :: tl ->
          if equal k k' then tl else (k', v') :: remove_box tl k ~equal
    in
    let rec add_box box k v ~equal =
      match box with
      | [] -> [(k, v)]
      | (k', v') :: tl ->
          if equal k k' then (k', v) :: tl
          else (k', v') :: add_box tl k v ~equal
    in
    while P.not_eof p do
      let h, i, chars = (ref 0, ref 0, ref []) in
      while
        match P.parse_char_u p with
        | '-' ->
            let chars = String.of_char_list !chars |> String.rev in
            boxes.(!h) <- remove_box boxes.(!h) ~equal chars ;
            false
        | '=' ->
            let n = P.parse_digit_u p in
            let chars = String.of_char_list !chars |> String.rev in
            boxes.(!h) <- add_box boxes.(!h) ~equal chars n ;
            false
        | c ->
            h := hash_char !h c ;
            i := !i * 256 * Char.to_int c ;
            chars := c :: !chars ;
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

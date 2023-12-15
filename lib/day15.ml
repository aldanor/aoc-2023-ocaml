open Core
open! Imports

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    let f (sum, hash) c =
      match c with
      | ',' -> (sum + hash, 0)
      | c -> (sum, (hash + Char.to_int c) * 17 mod 256)
    in
    String.fold s ~init:(0, 0) ~f |> (fun (a, b) -> a + b) |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ = "HASH" |> run_test ; [%expect {| 52 |}]

let%expect_test _ =
  "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" |> run_test ;
  [%expect {| 1320 |}]

open! Imports

module M = struct
  type t = string

  let parse s = s

  let part1 _ = ""

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ = "" |> run_test ; [%expect {| |}]

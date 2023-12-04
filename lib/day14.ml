open! Imports

module M = struct
  type t = unit

  let parse _inputs = ()

  let part1 _ = ""

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ = "" |> run_test ; [%expect {| |}]

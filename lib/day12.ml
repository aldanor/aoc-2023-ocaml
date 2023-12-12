open Core
open Imports

let _DEBUG = false

let pr fmt = if _DEBUG then printf fmt else ifprintf Out_channel.stdout fmt

let solve s start len to_place =
  let rec f i state to_place ~cps =
    if i < 0 then
      (List.is_empty to_place || (state = 0 && List.length to_place = 1))
      |> Bool.to_int |> cps
    else
      let j = i - 1 in
      match (s.[start + i], state, to_place) with
      | '#', _, [] -> cps 0
      | _, _, [] -> f j ~cps 0 []
      | '?', -1, x :: _ ->
          f j (x - 1) to_place ~cps:(fun x ->
              f j (-1) to_place ~cps:(fun y -> cps (x + y)) )
      | '#', -1, x :: _ -> f j ~cps (x - 1) to_place
      | '?', 0, _ :: xs | '.', 0, _ :: xs -> f j ~cps (-1) xs
      | '.', -1, xs -> f j ~cps (-1) xs
      | '#', 0, _ | '.', _, _ -> cps 0
      | '?', _, _ | '#', _, _ -> f j ~cps (state - 1) to_place
      | c, _, _ -> failwith @@ sprintf "unexpected char: '%c'" c
  in
  f (len - 1) (-1) to_place ~cps:(fun x -> x)

let solve_dumb s start len p =
  (* bruteforce solution of part 1 to check for correctness *)
  let s = String.sub s ~pos:start ~len in
  let n_questions = String.count s ~f:Char.(fun c -> c = '?') in
  let n_options = 1 lsl n_questions in
  let mutate_string option =
    let b = Bytes.of_string s in
    let i = ref 0 in
    for j = 0 to len - 1 do
      if Char.(Bytes.get b j = '?') then (
        Bytes.set b j (if option land (1 lsl !i) <> 0 then '#' else '.') ;
        incr i )
    done ;
    b
  in
  let is_valid b =
    let c_prev = ref '.' in
    let block, blocks = (ref 0, ref []) in
    for j = 0 to len - 1 do
      let c = Bytes.get b j in
      ( match (!c_prev, c) with
      | '#', '#' -> incr block
      | _, '#' -> block := 1
      | '#', _ ->
          blocks := !block :: !blocks ;
          block := 0
      | _ -> () ) ;
      c_prev := c
    done ;
    if !block <> 0 then blocks := !block :: !blocks ;
    List.equal Int.equal !blocks p
  in
  List.range 0 n_options
  |> List.map ~f:(fun option ->
         option |> mutate_string |> is_valid |> Bool.to_int )
  |> sum_ints

let solve_one s p = solve s 0 (String.length s) p
(* let solve_one s p = solve_dumb s 0 (String.length s) p *)

let%expect_test _ =
  printf "%d\n" @@ solve_one "???.###" [3; 1; 1] ;
  [%expect {| 1 |}]

let%expect_test _ =
  printf "%d\n" @@ solve_one ".??..??...?##." [3; 1; 1] ;
  [%expect {| 4 |}]

let%expect_test _ =
  printf "%d\n" @@ solve_one "?#?#?#?#?#?#?#?" [6; 1; 3; 1] ;
  [%expect {| 1 |}]

let%expect_test _ =
  printf "%d\n" @@ solve_one "????.#...#..." [1; 1; 4] ;
  [%expect {| 1 |}]

let%expect_test _ =
  printf "%d\n" @@ solve_one "????.######..#####." [5; 6; 1] ;
  [%expect {| 4 |}]

let%expect_test _ =
  printf "%d\n" @@ solve_one "?###????????" [1; 2; 3] ;
  [%expect {| 10 |}]

let%expect_test "foo" =
  printf "%d\n" @@ solve_one "?.?#????.?" [1; 3] ;
  [%expect {| 5 |}]

let parse_nums s i =
  let n = String.length s in
  let rec f' i num to_place =
    if i = n then (i, num, num :: to_place)
    else
      match s.[i] with
      | '0' .. '9' as c ->
          f' (i + 1) ((num * 10) + parse_digit_unchecked c) to_place
      | ',' -> f' (i + 1) 0 (num :: to_place)
      | _ -> (i, num, num :: to_place)
  in
  let i, _, to_place = f' i 0 [] in
  (i, to_place)

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 7204 *)
    let n = String.length s in
    let start = ref 0 in
    let ans = ref 0 in
    while !start < n do
      let i = String.index_from_exn s !start ' ' in
      let len = i - !start in
      let j, to_place = parse_nums s (i + 1) in
      ans := !ans + solve s !start len to_place ;
      start := j + 1
    done ;
    !ans |> Int.to_string

  let part2 s =
    String.rev (s ^ "\n") |> ignore ;
    ""
end

include M
include Day.Make (M)

(* let%expect_test _ = "" |> run_test ; [%expect {| |}] *)

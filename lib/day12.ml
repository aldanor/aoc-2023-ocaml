open Core
open Imports

let counts = Array.create ~len:16384 0

let longest = Array.create ~len:256 0

let solve_dp s start len blocks =
  let m, n = (Array.length blocks, len) in
  (* counts[i][j] is the number of ways to place the first `i` elements so
     that there's `j` blocks of '#'s whose lengths match the first `j`
     requested lengths *)
  Array.fill counts ~pos:0 ~len:((m + 1) * (n + 1)) 0 ;
  (* let counts = Array.create ~len:((m + 1) * (n + 1)) 0 in *)
  counts.(0) <- 1 ;
  (* next, precompute what's the longest block we can start at each point *)
  let longest = Array.create ~len:(n + 1) 0 in
  for i = n - 1 downto 0 do
    longest.(i) <-
      (if Char.(s.[start + i] = '.') then 0 else longest.(i + 1) + 1)
  done ;
  (* finally, fill in the counts *)
  let w = m + 1 in
  for i = 0 to n - 1 do
    let c = String.unsafe_get s (start + i) in
    let longest = Array.unsafe_get longest i in
    for j = 0 to m - 1 do
      let idx = (i * w) + j in
      let iadd i' j' =
        let k = (i' * w) + j' in
        Array.unsafe_set counts k
        @@ (Array.unsafe_get counts k + Array.unsafe_get counts idx)
      in
      let b = Array.unsafe_get blocks j in
      (* if we've already made `j` blocks and current char is '.' or '?', we
         can always make an '.' out of it and keep the same count *)
      if Char.(c <> '#') then iadd (i + 1) j ;
      (* next, check is we can place a new block here *)
      if
        longest >= b
        && i + b <= n
        && Char.(String.unsafe_get s (start + i + b) <> '#')
      then iadd (min (i + b + 1) n) (j + 1)
    done ;
    if Char.(c <> '#') then
      let k = ((i + 1) * w) + m in
      let v = counts.(k) in
      counts.(k) <- v + counts.((i * w) + m)
  done ;
  counts.((n * w) + m)

let test_solve s p = solve_dp (s ^ " ") 0 (String.length s) p

let%expect_test _ =
  printf "%d\n" @@ test_solve "???.###" [|1; 1; 3|] ;
  [%expect {| 1 |}]

let%expect_test _ =
  printf "%d\n" @@ test_solve ".??..??...?##." [|1; 1; 3|] ;
  [%expect {| 4 |}]

let%expect_test _ =
  printf "%d\n" @@ test_solve "?#?#?#?#?#?#?#?" [|1; 3; 1; 6|] ;
  [%expect {| 1 |}]

let%expect_test _ =
  printf "%d\n" @@ test_solve "????.#...#..." [|4; 1; 1|] ;
  [%expect {| 1 |}]

let%expect_test _ =
  printf "%d\n" @@ test_solve "????.######..#####." [|1; 6; 5|] ;
  [%expect {| 4 |}]

let%expect_test _ =
  printf "%d\n" @@ test_solve "?###????????" [|3; 2; 1|] ;
  [%expect {| 10 |}]

let%expect_test "foo" =
  printf "%d\n" @@ test_solve "?.?#????.?" [|3; 1|] ;
  [%expect {| 5 |}]

let parse_nums s i =
  let n = String.length s in
  let rec f' i num blocks =
    if i = n then (i, num, num :: blocks)
    else
      match s.[i] with
      | '0' .. '9' as c ->
          f' (i + 1) ((num * 10) + parse_digit_unchecked c) blocks
      | ',' -> f' (i + 1) 0 (num :: blocks)
      | _ -> (i, num, num :: blocks)
  in
  let i, _, blocks = f' i 0 [] in
  (i, blocks |> Array.of_list_rev)

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 7204 *)
    let start, ans = (ref 0, ref 0) in
    while !start < String.length s do
      let i = String.index_from_exn s !start ' ' in
      let j, blocks = parse_nums s (i + 1) in
      ans := !ans + solve_dp s !start (i - !start) blocks ;
      start := j + 1
    done ;
    !ans |> Int.to_string

  let part2 s =
    (* 1672318386674 *)
    let start, ans = (ref 0, ref 0) in
    while !start < String.length s do
      let i = String.index_from_exn s !start ' ' in
      let j, blocks = parse_nums s (i + 1) in
      let len = i - !start in
      let s = String.sub s ~pos:!start ~len in
      let s =
        String.concat
          (List.init 6 ~f:(fun i -> if i = 5 then "" else s))
          ~sep:"?"
      in
      let blocks = Array.concat (List.init 5 ~f:(fun _ -> blocks)) in
      ans := !ans + solve_dp s 0 ((len * 5) + 4) blocks ;
      start := j + 1
    done ;
    !ans |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "???.### 1,1,3\n\
   .??..??...?##. 1,1,3\n\
   ?#?#?#?#?#?#?#? 1,3,1,6\n\
   ????.#...#... 4,1,1\n\
   ????.######..#####. 1,6,5\n\
   ?###???????? 3,2,1" |> run_test ;
  [%expect {| 21 525152 |}]

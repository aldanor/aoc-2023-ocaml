open Core

let get_width s =
  (* doesn't include the trailing newline *)
  String.findi s ~f:(fun _ c -> Char.(c = '\n')) |> Option.value_exn |> fst

let pad_input s =
  let w = 1 + get_width s in
  let w_pad = w + 1 in
  let pad = String.make w_pad '.' in
  let start, len = (w_pad, String.length s) in
  let s = String.concat [pad; s; pad] in
  (s, start, len, w)

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 525181 *)
    let s, start, len, w = pad_input s in
    let is_sym i =
      i |> String.unsafe_get s
      |> function '.' | '0' .. '9' | '\n' -> false | _ -> true
    in
    let get_digit i = i |> String.unsafe_get s |> Char.get_digit in
    let check_left i =
      let u, d = (i - w, i + w) in
      is_sym (u - 1)
      || is_sym u
      || is_sym (i - 1)
      || is_sym (d - 1)
      || is_sym d
    in
    let check_right i =
      let r = i + 1 in
      is_sym (r - w) || is_sym r || is_sym (r + w)
    in
    let running, neighbors, total = (ref None, ref false, ref 0) in
    for i = start to start + (len - 1) + 1 do
      match (get_digit i, !running) with
      | None, None -> ()
      | None, Some r ->
          running := None ;
          total := !total + if !neighbors then r else 0
      | Some d, None ->
          running := Some d ;
          neighbors := check_left i || check_right i
      | Some d, Some r ->
          running := Some ((r * 10) + d) ;
          neighbors := !neighbors || check_right i
    done ;
    !total |> Int.to_string

  let part2 s =
    (* 84289137 *)
    let s, start, len, w = pad_input s in
    let is_digit i = i |> String.unsafe_get s |> Char.is_digit in
    let get_digit i = i |> String.unsafe_get s |> Char.get_digit in
    let neighbors3 i out =
      let l, m, r = (i - 1, i, i + 1) in
      match (is_digit l, is_digit m, is_digit r) with
      | false, false, true -> r :: out
      | false, true, _ -> m :: out
      | true, false, true -> l :: r :: out
      | true, _, _ -> l :: out
      | _ -> out
    in
    let neighbor1 i out = if is_digit i then i :: out else out in
    let find_neighbors i =
      []
      |> neighbors3 (i - w)
      |> neighbors3 (i + w)
      |> neighbor1 (i - 1)
      |> neighbor1 (i + 1)
    in
    let rec number_backtrack i =
      match is_digit (i - 1) with
      | false -> i
      | true -> number_backtrack (i - 1)
    in
    let parse_num i =
      let rec loop acc j =
        match get_digit j with
        | None -> acc
        | Some d -> loop ((acc * 10) + d) (j + 1)
      in
      number_backtrack i |> loop 0
    in
    let ans = ref 0 in
    for i = start to start + len do
      if Char.(String.unsafe_get s i = '*') then
        match find_neighbors i with
        | [a; b] -> ans := !ans + (parse_num a * parse_num b)
        | _ -> ()
    done ;
    !ans |> Int.to_string
end

include M
include Day.Make (M)

let example =
  "467..114..\n\
   ...*......\n\
   ..35..633.\n\
   ......#...\n\
   617*......\n\
   .....+.58.\n\
   ..592.....\n\
   ......755.\n\
   ...$.*....\n\
   .664.598.."

let%expect_test _ =
  example |> parse |> part1 |> printf "%s" ;
  [%expect {| 4361 |}]

let%expect_test _ =
  example |> parse |> part2 |> printf "%s" ;
  [%expect {| 467835 |}]

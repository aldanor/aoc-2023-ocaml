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
    let is_symbol i =
      i |> String.get s
      |> function '.' | '0' .. '9' | '\n' -> false | _ -> true
    in
    let get_digit i = i |> String.get s |> Char.get_digit in
    let offsets_left = [-w - 1; -w; -1; w - 1; w] in
    let offsets_right = [-w + 1; 1; w + 1] in
    let has_neighbors o i = List.exists o ~f:(fun o -> is_symbol (i + o)) in
    let check_left = has_neighbors offsets_left in
    let check_right = has_neighbors offsets_right in
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
    let is_digit i = i |> String.get s |> Char.is_digit in
    let offsets =
      [ (-w - 1, false)
      ; (-w, true)
      ; (-w + 1, true)
      ; (-1, false)
      ; (1, false)
      ; (w - 1, false)
      ; (w, true)
      ; (w + 1, true) ]
    in
    let neighboring_numbers i =
      List.fold offsets ~init:(false, [])
        ~f:(fun (prev_digit, acc) (o, contiguous) ->
          let j = i + o in
          match (is_digit j, prev_digit, contiguous) with
          | false, _, _ -> (false, acc)
          | true, false, _ -> (true, j :: acc)
          | true, true, true -> (true, acc)
          | true, true, false -> (true, j :: acc) )
      |> snd
    in
    let rec number_backtrack i =
      match is_digit (i - 1) with
      | false -> i
      | true -> number_backtrack (i - 1)
    in
    let parse_num i =
      let get_digit j = j |> String.get s |> Char.get_digit in
      let rec loop acc j =
        match get_digit j with
        | None -> acc
        | Some d -> loop ((acc * 10) + d) (j + 1)
      in
      number_backtrack i |> loop 0
    in
    Sequence.range start (start + len)
    |> Sequence.filter ~f:(fun i -> Char.(String.get s i = '*'))
    |> Sequence.map ~f:neighboring_numbers
    |> Sequence.filter_map ~f:(function [a; b] -> Some (a, b) | _ -> None)
    |> Sequence.fold ~init:0 ~f:(fun acc (a, b) ->
           acc + (parse_num a * parse_num b) )
    |> Int.to_string
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

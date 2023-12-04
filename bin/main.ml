open Aoc
open Core
open Stdio
open Utils

let get_day_module day =
  let modules : (module Day.S) list =
    [ (module Day01)
    ; (module Day02)
    ; (module Day03)
    ; (module Day04)
    ; (module Day05)
    ; (module Day06)
    ; (module Day07)
    ; (module Day08)
    ; (module Day09)
    ; (module Day10)
    ; (module Day11)
    ; (module Day12)
    ; (module Day13)
    ; (module Day14)
    ; (module Day15)
    ; (module Day16)
    ; (module Day17)
    ; (module Day18)
    ; (module Day19)
    ; (module Day20)
    ; (module Day21)
    ; (module Day22)
    ; (module Day23)
    ; (module Day24)
    ; (module Day25) ]
  in
  List.nth_exn modules (day - 1)

let read_inputs day =
  let input_file = sprintf "inputs/%02d.in" day in
  if not (Sys_unix.file_exists_exn input_file) then
    download_input day input_file ;
  In_channel.read_all input_file

let run_single ~day ~bench ~part1 ~part2 =
  let _ = (bench, part1, part2) in
  let inputs = read_inputs day in
  let (module Day : Day.S) = get_day_module day in
  if bench then Day.bench ~only1:part1 ~only2:part2 inputs
  else Day.run inputs

let run_command ~day ~bench ~part1 ~part2 =
  let days =
    day |> Option.value_map ~default:(List.range 1 26) ~f:List.return
  in
  let part1, part2 =
    match (part1, part2) with false, false -> (true, true) | parts -> parts
  in
  List.iter days ~f:(fun day -> run_single ~day ~bench ~part1 ~part2)

let () =
  let param_day =
    Command.Arg_type.create (fun day ->
        match Int.of_string_opt day with
        | Some day when day >= 1 && day <= 25 -> day
        | _ -> failwith "expected an integer between 1 and 25" )
  in
  let command =
    Command.basic ~summary:"AoC 2023"
      (let%map_open.Command day = anon (maybe ("day" %: param_day))
       and bench = flag "-b" no_arg ~doc:" Run benchmarks"
       and part1 = flag "-1" no_arg ~doc:" Part 1 only"
       and part2 = flag "-2" no_arg ~doc:" Part 2 only" in
       fun () -> run_command ~day ~bench ~part1 ~part2 )
  in
  Command_unix.run command

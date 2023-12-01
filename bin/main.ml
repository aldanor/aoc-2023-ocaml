open Aoc
open Utils

let () =
  let args = Sys.argv in
  let day = args.(1) in
  let input_file = Printf.sprintf "inputs/%s.in" day in
  if not @@ Stdlib.Sys.file_exists input_file then
    download_input day input_file ;
  let file = In_channel.open_text input_file in
  let inputs = In_channel.input_all file in
  let (module Day : Day.S) =
    match day with
    | "01" -> (module Day01)
    | "02" -> (module Day02)
    | "03" -> (module Day03)
    | "04" -> (module Day04)
    | "05" -> (module Day05)
    | "06" -> (module Day06)
    | "07" -> (module Day07)
    | "08" -> (module Day08)
    | "09" -> (module Day09)
    | "10" -> (module Day10)
    | "11" -> (module Day11)
    | "12" -> (module Day12)
    | "13" -> (module Day13)
    | "14" -> (module Day14)
    | "15" -> (module Day15)
    | "16" -> (module Day16)
    | "17" -> (module Day17)
    | "18" -> (module Day18)
    | "19" -> (module Day19)
    | "20" -> (module Day20)
    | "21" -> (module Day21)
    | "22" -> (module Day22)
    | "23" -> (module Day23)
    | "24" -> (module Day24)
    | "25" -> (module Day25)
    | _ -> failwith "invalid day"
  in
  Day.run inputs ; In_channel.close file

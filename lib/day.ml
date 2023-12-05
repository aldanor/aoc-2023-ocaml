open Core

module type S = sig
  val run : ?only1:bool -> ?only2:bool -> string -> unit

  val run_test : ?part:int -> string -> unit

  val bench :
       ?name:string
    -> ?only1:bool
    -> ?only2:bool
    -> ?parse:bool
    -> ?quota:string
    -> string
    -> unit
end

module type Impl = sig
  type t

  val parse : string -> t

  val part1 : t -> string

  val part2 : t -> string
end

module Make (Impl : Impl) : S = struct
  let run ?(only1 = false) ?(only2 = false) inputs =
    let parsed = Impl.parse inputs in
    if not only2 then parsed |> Impl.part1 |> printf "%s" |> ignore ;
    if not only1 then (
      if not only2 then printf " " |> ignore ;
      parsed |> Impl.part2 |> printf "%s" |> ignore ) ;
    if (not only1) || not only2 then printf "\n" |> ignore

  let run_test ?(part = 0) inputs =
    let p = Impl.parse inputs in
    match part with
    | 1 -> Impl.part1 p |> printf "%s\n" |> ignore
    | 2 -> Impl.part2 p |> printf "%s\n" |> ignore
    | _ ->
        Impl.part1 p |> printf "%s" |> ignore ;
        Impl.part2 p |> printf " %s\n" |> ignore

  let bench ?(name = "") ?(only1 = false) ?(only2 = false) ?(parse = false)
      ?(quota = "1s") inputs =
    let name s = if String.is_empty name then s else name ^ " :: " ^ s in
    let parsed = Impl.parse inputs in
    let fn_parse () = Impl.parse inputs |> ignore in
    let fn_part1 () = Impl.part1 parsed |> ignore in
    let fn_part2 () = Impl.part2 parsed |> ignore in
    let fns = ref [] in
    if not only1 then fns := (name "part 2", fn_part2) :: !fns ;
    if not only2 then fns := (name "part 1", fn_part1) :: !fns ;
    if parse then fns := (name "parse", fn_parse) :: !fns ;
    let open Core_bench_internals in
    let open Core_bench in
    let benches =
      List.map !fns ~f:(fun (name, fn) -> Bench.Test.create fn ~name)
    in
    let verbosity = Verbosity.Quiet in
    let quota = Quota.of_string quota in
    let run_config = Run_config.create () ~verbosity ~quota in
    let measurements = Bench.measure ~run_config benches in
    let analysis_configs = [Bench.Analysis_config.nanos_vs_runs] in
    let analyze m =
      m |> Bench.analyze ~analysis_configs |> Or_error.ok_exn
    in
    let results = List.map measurements ~f:analyze in
    let display_config =
      Bench.Display_config.create
        ~display:(Defaults.string_to_display "blank")
        ()
    in
    Bench.display results ~display_config
end

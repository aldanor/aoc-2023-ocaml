open Core

module type S = sig
  val run : ?only_part1:bool -> ?only_part2:bool -> string -> unit
end

module type Impl = sig
  type t

  val parse : string -> t

  val part1 : t -> string

  val part2 : t -> string
end

let run_solution f =
  let ans = Imports.time_m f in
  printf "%s\n" ans

module Make (Impl : Impl) : S = struct
  let run ?(only_part1 = false) ?(only_part2 = false) inputs =
    let parsed = Impl.parse inputs in
    let () =
      if not only_part2 then run_solution (fun () -> Impl.part1 parsed)
    in
    let () =
      if not only_part1 then run_solution (fun () -> Impl.part2 parsed)
    in
    ()
end

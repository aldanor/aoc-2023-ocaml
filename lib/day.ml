module type S = sig
  val run : ?only_part1:bool -> ?only_part2:bool -> string -> unit
end

module type Impl = sig
  type t

  val parse : string -> t

  val part1 : t -> unit

  val part2 : t -> unit
end

module Make (Impl : Impl) : S = struct
  let run ?(only_part1 = false) ?(only_part2 = false) inputs =
    let parsed = Impl.parse inputs in
    let () =
      if not only_part2 then Imports.time_m (fun () -> Impl.part1 parsed)
    in
    let () =
      if not only_part1 then Imports.time_m (fun () -> Impl.part2 parsed)
    in
    ()
end

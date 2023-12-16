open Core

module Board = struct
  type t = {s: string; v: bytes; w: int; h: int}

  type state = {x: int; y: int; pos: int; dir: int}

  let width b = b.w - 1

  let height b = b.h

  let of_string s =
    let w = String.index_exn s '\n' in
    let n = String.length s in
    let h = (n + 1) / (w + 1) in
    let v = Bytes.create (n + 1) in
    Bytes.fill v ~pos:0 ~len:(n + 1) '\x00' ;
    {s; v; w= w + 1; h}

  let start {w; _} x y dir = {x; y; pos= (y * w) + x; dir}

  let move w {x; y; pos; dir} =
    match dir with
    | 0 -> {x; y= y - 1; pos= pos - w; dir}
    | 1 -> {x= x + 1; y; pos= pos + 1; dir}
    | 2 -> {x; y= y + 1; pos= pos + w; dir}
    | _ -> {x= x - 1; y; pos= pos - 1; dir}

  let pprint {s; v; _} =
    for i = 0 to String.length s - 1 do
      let s = String.get s i in
      let v = Char.to_int (Bytes.get v i) in
      printf "%c"
      @@
      match (s, v) with
      | '.', 1 -> '^'
      | '.', 2 -> '>'
      | '.', 4 -> 'v'
      | '.', 8 -> '<'
      | '.', v when v <> 0 ->
          Int.popcount v + Char.to_int '0' |> Char.of_int_exn
      | _ -> s
    done ;
    print_endline ""

  let reset {v; _} = Bytes.fill v ~pos:0 ~len:(Bytes.length v) '\x00'

  let run {s; v; w; h} start =
    (* nesw = 0123 *)
    (* reverse = 2301 = (dir + 2) mod 4 *)
    (* \ = wsen = 3210 = 3 - dir *)
    (* / = enws = 1032 = reverse(3 - dir) = (5 - dir) mod 4 *)
    (* | := dir if dir == 0|2 else [/, \] *)
    (* - := dir if dir == 1|3 else [/, \] *)
    (* ord(|) mod 2 = 1, ord(-) mod 2 = 0 *)
    let rec f queue terminal =
      match queue with
      | [] -> terminal
      | {x; y; _} :: qs when x < 0 -> f qs ((3, y) :: terminal) (* w *)
      | {x; y; _} :: qs when x >= w - 1 -> f qs ((1, y) :: terminal) (* e *)
      | {x; y; _} :: qs when y < 0 -> f qs ((0, x) :: terminal) (* n *)
      | {x; y; _} :: qs when y >= h -> f qs ((2, x) :: terminal) (* s *)
      | {pos; dir; _} :: qs
        when Char.to_int (Bytes.unsafe_get v pos) land (1 lsl dir) <> 0 ->
          f qs terminal
      | ({pos; dir; _} as state) :: qs ->
          Bytes.unsafe_set v pos
          @@ Char.unsafe_of_int
               (Char.to_int (Bytes.unsafe_get v pos) lor (1 lsl dir)) ;
          let c = String.unsafe_get s pos in
          let move_fwd () = move w {state with dir= (5 - dir) land 3} in
          let move_rev () = move w {state with dir= 3 - dir} in
          let queue =
            match c with
            | '\\' -> move_rev () :: qs
            | '/' -> move_fwd () :: qs
            | ('-' | '|') when dir land 1 <> Char.to_int c land 1 ->
                move_rev () :: move_fwd () :: qs
            | _ -> move w state :: qs
          in
          f queue terminal
    in
    f [start] []

  let n_energized {v; _} =
    Bytes.fold v ~init:0 ~f:(fun acc c ->
        acc + (Char.(c <> '\x00') |> Bool.to_int) )
end

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 7472 *)
    let module B = Board in
    let b = B.of_string s in
    B.run b (B.start b 0 0 1) |> ignore ;
    B.n_energized b |> Int.to_string

  let part2 s =
    (* 7716 *)
    let module B = Board in
    let b = B.of_string s in
    let w, h = (B.width b, B.height b) in
    let starts =
      [| Array.init w ~f:(fun x -> Some (B.start b x 0 2))
       ; Array.init h ~f:(fun y -> Some (B.start b (w - 1) y 3))
       ; Array.init w ~f:(fun x -> Some (B.start b x (h - 1) 0))
       ; Array.init h ~f:(fun y -> Some (B.start b 0 y 1)) |]
    in
    let n_max = ref 0 in
    for i = 0 to 3 do
      for j = 0 to Array.length starts.(i) - 1 do
        match starts.(i).(j) with
        | Some start ->
            B.reset b ;
            let terminal = B.run b start in
            List.iter terminal ~f:(fun (dir, loc) ->
                starts.(dir).(loc) <- None ) ;
            n_max := max !n_max (B.n_energized b)
        | None -> ()
      done
    done ;
    !n_max |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  ".|...\\....\n\
   |.-.\\.....\n\
   .....|-...\n\
   ........|.\n\
   ..........\n\
   .........\\\n\
   ..../.\\\\..\n\
   .-.-/..|..\n\
   .|....-|.\\\n\
   ..//.|...." |> run_test ;
  [%expect {| 46 51 |}]

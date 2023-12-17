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

type interval = {pos: int; len: int; step: int}
[@@deriving show {with_path= false}]

type direction = int

type node = Terminal of terminal_node | Branch of branch_node

and terminal_node = {dir: int; coord: int}

and branch_node =
  { mutable visited: bool
  ; mutable left: node_with_path
  ; mutable right: node_with_path }

and node_with_path = {mutable node: node; mutable intervals: interval list}

type map = {s: string; w: int; h: int}

let map_of_string s =
  let w = String.index_exn s '\n' in
  let n = String.length s in
  let h = (n + 1) / (w + 1) in
  {s; w= w + 1; h}

type location = {x: int; y: int; pos: int; dir: int}
[@@deriving show {with_path= false}]

let intervals_to_string = List.to_string ~f:show_interval

let _N, _E, _S, _W = (0, 1, 2, 3)

let xyd {w; _} x y dir = {x; y; pos= (y * w) + x; dir}

let branch_iter {left; right; _} ~f = f left ; f right

let traverse_map {s; w; h} loc =
  (* walk until we hit the edge (First) or a branch (Second) *)
  let move {x; y; pos; dir} =
    match dir with
    | 0 -> {x; y= y - 1; pos= pos - w; dir}
    | 1 -> {x= x + 1; y; pos= pos + 1; dir}
    | 2 -> {x; y= y + 1; pos= pos + w; dir}
    | _ -> {x= x - 1; y; pos= pos - 1; dir}
  in
  let move_fwd loc = move {loc with dir= (5 - loc.dir) land 3} in
  let move_rev loc = move {loc with dir= 3 - loc.dir} in
  let interval loc start =
    let len, step =
      if loc.y = start.y then (abs (loc.x - start.x), 1)
      else (abs (loc.y - start.y), w)
    in
    {pos= min loc.pos start.pos; len= len + 1; step}
  in
  let rec f loc start intervals =
    let make_intervals loc =
      if start.x >= 0 && start.x < w - 1 && start.y >= 0 && start.y < h then
        interval loc start :: intervals
      else intervals
    in
    match loc with
    | {x; y; pos; _} when x < 0 ->
        (make_intervals {loc with x= x + 1; pos= pos + 1}, First (_W, y))
    | {x; y; pos; _} when x >= w - 1 ->
        (make_intervals {loc with x= x - 1; pos= pos - 1}, First (_E, y))
    | {x; y; pos; _} when y < 0 ->
        (make_intervals {loc with y= y + 1; pos= pos + w}, First (_N, x))
    | {x; y; pos; _} when y >= h ->
        (make_intervals {loc with y= y - 1; pos= pos - w}, First (_S, x))
    | {pos; dir; _} as loc -> (
      match String.get s pos with
      | ('\\' | '/') as c ->
          let dst = if Char.(c = '\\') then move_rev loc else move_fwd loc in
          f dst dst (make_intervals loc)
      | ('-' | '|') as c when dir land 1 <> Char.to_int c land 1 ->
          (make_intervals loc, Second loc)
      | _ -> f (move loc) start intervals )
  in
  f loc loc []

let traverse_result_to_string = function
  | intervals, First (dir, coord) ->
      sprintf "Terminal(dir: %d, coord: %d), intervals: %s" dir coord
        (intervals_to_string intervals)
  | intervals, Second loc ->
      sprintf "Branch(%s), intervals: %s" (show_location loc)
        (intervals_to_string intervals)

let build_branches {s; w; h} =
  let n = String.length s in
  let branch_ids, n_branches, branch_locs =
    (Array.create ~len:n 0, ref 0, ref [])
  in
  let pos = ref 0 in
  for y = 0 to h - 1 do
    for x = 0 to w - 2 do
      (let pos = !pos in
       match String.get s pos with
       | ('|' | '-') as c ->
           branch_ids.(pos) <- !n_branches ;
           incr n_branches ;
           let dir = if Char.(c = '|') then _N else _E in
           branch_locs := {x; y; pos; dir} :: !branch_locs
       | _ -> () ) ;
      incr pos
    done ;
    incr pos
  done ;
  let n_branches = !n_branches in
  let dummy () = {node= Terminal {dir= 0; coord= 0}; intervals= []} in
  let branches =
    Array.init n_branches ~f:(fun _ ->
        {visited= false; left= dummy (); right= dummy ()} )
  in
  let traverse_to_node loc =
    let intervals, dst = traverse_map {s; w; h} loc in
    match dst with
    | First (dir, coord) -> {node= Terminal {dir; coord}; intervals}
    | Second {pos; _} -> {node= Branch branches.(branch_ids.(pos)); intervals}
  in
  List.iter !branch_locs ~f:(fun loc ->
      let branch = branches.(branch_ids.(loc.pos)) in
      branch.left <- traverse_to_node loc ;
      branch.right <- traverse_to_node {loc with dir= loc.dir + 2} ) ;
  (branches, branch_ids)

let reset_branches branches =
  Array.iter branches ~f:(fun branch -> branch.visited <- false)

let traverse_branch branch ~f =
  let terminal = ref [] in
  let rec g branch =
    if branch.visited then ()
    else (
      branch.visited <- true ;
      branch_iter branch ~f:(fun b ->
          b.intervals |> List.iter ~f ;
          match b.node with
          | Terminal t -> terminal := t :: !terminal
          | Branch branch -> g branch ) )
  in
  g branch ; !terminal

let apply_interval {pos; len; step} (mask : bytes) =
  let pos = ref pos in
  for _ = 1 to len do
    Bytes.set mask !pos '\x01' ;
    pos := !pos + step
  done

let reset_mask mask = Bytes.fill mask ~pos:0 ~len:(Bytes.length mask) '\x00'

let new_mask n =
  let mask = Bytes.create n in
  reset_mask mask ; mask

let count_mask mask =
  Bytes.fold mask ~init:0 ~f:(fun acc c -> acc + Char.to_int c)

let traverse_full map branches branch_ids loc ~f =
  let intervals, res = traverse_map map loc in
  intervals |> List.iter ~f ;
  match res with
  | First (dir, coord) -> [{dir; coord}]
  | Second {pos; _} -> traverse_branch branches.(branch_ids.(pos)) ~f

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
    let m = map_of_string s in
    let branches, branch_ids = build_branches m in
    let starts =
      [| Array.init m.w ~f:(fun x -> Some (xyd m x 0 2))
       ; Array.init m.h ~f:(fun y -> Some (xyd m (m.w - 1) y 3))
       ; Array.init m.w ~f:(fun x -> Some (xyd m x (m.h - 1) 0))
       ; Array.init m.h ~f:(fun y -> Some (xyd m 0 y 1)) |]
    in
    let mask = new_mask (String.length s) in
    let n_max = ref 0 in
    for i = 0 to 3 do
      for j = 0 to Array.length starts.(i) - 1 do
        match starts.(i).(j) with
        | Some start ->
            reset_mask mask ;
            reset_branches branches ;
            let terminal =
              traverse_full m branches branch_ids start ~f:(fun interval ->
                  apply_interval interval mask )
            in
            List.iter terminal ~f:(fun {dir; coord} ->
                starts.(dir).(coord) <- None ) ;
            n_max := max !n_max (count_mask mask)
        | None -> ()
      done
    done ;
    !n_max |> Int.to_string
end

let part2_simple s =
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

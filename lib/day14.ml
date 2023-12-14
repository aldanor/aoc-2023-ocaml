open Core

module Board = struct
  type t =
    { a: bytes
    ; w: int
    ; h: int
    ; mutable dir: int
    ; mutable hash: int
    ; mutable steps: int }

  let equal b1 b2 = b1.hash = b2.hash && Bytes.equal b1.a b2.a

  let copy b = {b with a= Bytes.copy b.a}

  let parse s =
    let w = String.index_exn s '\n' in
    let h = (String.length s + 1) / (w + 1) in
    let a = Bytes.create (w * h) in
    let n = ref 0 in
    for i = 0 to h - 1 do
      let iw = i * w in
      for j = 0 to w - 1 do
        let k = iw + j in
        let c = s.[k + i] in
        Bytes.set a k c ;
        n := !n + Bool.to_int Char.(c = 'O')
      done
    done ;
    (* first shift will be north, so initial dir is west *)
    let dir = 3 in
    (* we don't initialize hash at the start *)
    {a; w; h; dir; hash= 0; steps= 0}

  let pprint board =
    let {a; w; h; _} = board in
    printf "%s\n" @@ String.init w ~f:(fun _ -> '=') ;
    for i = 0 to h - 1 do
      for j = 0 to w - 1 do
        printf "%c" (Bytes.get a ((i * w) + j))
      done ;
      printf "\n"
    done ;
    printf "%s\n" @@ String.init w ~f:(fun _ -> '=')

  let iter_steps {dir; w; h; _} =
    (* n=0, w=1, s=2, e=3 *)
    match dir with
    | 0 -> (0, (w, h), (1, w))
    | 1 -> (0, (1, w), (w, h))
    | 2 -> ((h - 1) * w, (-w, h), (1, w))
    | _ -> (w - 1, (-1, w), (w, h))

  let shift_once board =
    let {a; _} = board in
    let dir = (board.dir + 1) mod 4 in
    board.dir <- dir ;
    board.steps <- board.steps + 1 ;
    let start, (di, ni), (dj, nj) = iter_steps board in
    let b = Array.create ~len:nj 0 in
    for j = 0 to nj - 1 do
      Array.unsafe_set b j (start + (dj * j))
    done ;
    board.hash <- 0 ;
    for i = 0 to ni - 1 do
      let k = ref (start + (di * i)) in
      for j = 0 to nj - 1 do
        (let k = !k in
         match Bytes.unsafe_get a k with
         | 'O' ->
             let dst = Array.unsafe_get b j in
             Bytes.unsafe_set a k '.' ;
             Bytes.unsafe_set a dst 'O' ;
             Array.unsafe_set b j (dst + di) ;
             board.hash <- board.hash lxor dst
         | '#' -> Array.unsafe_set b j (k + di)
         | _ -> () ) ;
        k := !k + dj
      done
    done

  let shift_cycle board =
    for _ = 0 to 3 do
      shift_once board
    done

  let load {w; h; a; _} =
    (* load is on the north support beams only, regardless of dir *)
    let load = ref 0 in
    for i = 0 to h - 1 do
      let iw = i * w in
      for j = 0 to w - 1 do
        if Char.(Bytes.get a (iw + j) = 'O') then load := !load + (h - i)
      done
    done ;
    !load
end

let brent ~init ~copy ~next ~equal =
  (* returns: (first_repeated_state, cycle_length) *)
  let power, lambda = (ref 1, ref 1) in
  let tortoise, hare = (ref (copy init), copy init) in
  next hare ;
  while not (equal !tortoise hare) do
    if !power = !lambda then (
      tortoise := copy hare ;
      power := !power * 2 ;
      lambda := 0 ) ;
    next hare ;
    incr lambda
  done ;
  let tortoise, hare = (copy init, copy init) in
  for _ = 0 to !lambda - 1 do
    next hare
  done ;
  while not (equal tortoise hare) do
    next tortoise ; next hare
  done ;
  (tortoise, !lambda)

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 110090 *)
    let w = String.index_exn s '\n' in
    let h = (String.length s + 1) / (w + 1) in
    let a = Array.create ~len:w 0 in
    let count, load = (ref 0, ref 0) in
    for i = 0 to h - 1 do
      let offset = i * (w + 1) in
      for j = 0 to w - 1 do
        match s.[offset + j] with
        | 'O' ->
            incr count ;
            load := !load + a.(j) ;
            a.(j) <- a.(j) + 1
        | '#' -> a.(j) <- i + 1
        | _ -> ()
      done
    done ;
    (!count * h) - !load |> Int.to_string

  let part2 s =
    (* 95254 *)
    let init = Board.parse s in
    let state, len =
      brent ~init ~copy:Board.copy ~next:Board.shift_cycle ~equal:Board.equal
    in
    for _ = 1 to (1_000_000_000 - (state.steps / 4)) mod len do
      Board.shift_cycle state
    done ;
    Board.load state |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "O....#....\n\
   O.OO#....#\n\
   .....##...\n\
   OO.#O....O\n\
   .O.....O#.\n\
   O.#..O.#.#\n\
   ..O..#O..O\n\
   .......O..\n\
   #....###..\n\
   #OO..#...." |> run_test ;
  [%expect {| 136 64 |}]

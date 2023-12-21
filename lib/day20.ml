open Core

type module_ =
  { mutable id: int
  ; mutable is_ff: bool
  ; mutable out: int list
  ; mutable inputs: int list }
[@@deriving show {with_path= false}]

let parse_inputs s =
  let module P = Imports.StreamParser in
  let p = P.create s in
  let modules =
    Array.init (26 * 26) ~f:(fun _ ->
        {id= -1; is_ff= true; out= []; inputs= []} )
  in
  let module_ids = Int.Table.create () in
  Hashtbl.add_exn module_ids ~key:(-1) ~data:0 ;
  let parse_letter c = Char.to_int c - Char.to_int 'a' in
  let parse_module () =
    let i = ref (parse_letter (P.parse_char_u p)) in
    while P.not_eof p && P.is_hd_lowercase_u p do
      i := (!i * 26) + parse_letter (P.parse_char_u p)
    done ;
    let i = !i in
    let index =
      Hashtbl.find_or_add module_ids i ~default:(fun () ->
          Hashtbl.length module_ids )
    in
    index
  in
  let parse_out () =
    let out = ref [] in
    while P.not_newline p do
      out := parse_module () :: !out ;
      if P.not_eof p && Char.(P.hd_u p = ',') then P.skip p 2
    done ;
    List.rev !out
  in
  while P.not_eof p do
    let i =
      match P.parse_char_u p with
      | 'b' -> P.skip_to p '>' ; P.skip p 2 ; 0
      | c ->
          let is_ff = Char.(c = '%') in
          let i = parse_module () in
          modules.(i).is_ff <- is_ff ;
          P.skip p 4 ;
          i
    in
    let out = parse_out () in
    modules.(i).out <- out ;
    List.iter out ~f:(fun j ->
        modules.(j).inputs <- i :: modules.(j).inputs ) ;
    P.skip p 1
  done ;
  let len = Hashtbl.length module_ids in
  for i = 0 to len - 1 do
    modules.(i).id <- i
  done ;
  Array.sub modules ~pos:0 ~len

type system =
  { modules: module_ array
  ; is_rxc: int array
  ; rxc_masks: int array
  ; rx_mask: int }
[@@deriving show {with_path= false}]

let parse_and_validate s =
  let modules = parse_inputs s in
  let n = Array.length modules in
  (* unfortunately, we have to assume specific input structure since that
     seems to be a part of the problem; it's ugly but that's what it takes,
     it seems... *)
  (* verify the following structure: *)
  (* - there's less than 64 nodes total *)
  (* - there's only one node with no outputs (rx) *)
  (* - rx must have a single input (rxa), of type & with 4 inputs *)
  (* - rxa has 4 inputs (rxb), each of type & with 1 input (rxc) *)
  (* - all of rxc must be of type & and their inputs don't overlap  *)
  (* - there are no more conjunction nodes in the system *)
  (* ... then: *)
  (* for rx to receive low, all rxa's inputs must be high *)
  (* for rxb to send high, a matching rxc must send low *)
  (* for rxc to send low, all of its inputs must be high *)
  let no_outputs m = List.is_empty m.out in
  let is_conjunction m = not m.is_ff in
  let has_inputs n m = List.length m.inputs = n in
  assert (Array.length modules < 64) ;
  assert (Array.count modules ~f:no_outputs = 1) ;
  let rx = Array.find_exn modules ~f:no_outputs in
  assert (has_inputs 1 rx) ;
  let rxa = modules.(List.hd_exn rx.inputs) in
  assert (is_conjunction rxa) ;
  assert (has_inputs 4 rxa) ;
  let rxb = List.map rxa.inputs ~f:(Array.get modules) |> Array.of_list in
  Array.iter rxb ~f:(fun m -> assert (is_conjunction m && has_inputs 1 m)) ;
  let rxc = Array.map rxb ~f:(fun m -> modules.(List.hd_exn m.inputs)) in
  Array.iter rxc ~f:(fun m -> assert (is_conjunction m)) ;
  let rxc_masks = Array.create ~len:4 0 in
  for i = 0 to 3 do
    rxc_masks.(i) <-
      List.fold rxc.(i).inputs ~init:0 ~f:(fun acc j -> acc lor (1 lsl j))
  done ;
  let rxc_m1_sum =
    Array.fold rxc_masks ~init:0 ~f:(fun acc m -> acc + Int.popcount m)
  in
  let rxc_m1_lor =
    Int.popcount (Array.fold rxc_masks ~init:0 ~f:(fun acc m -> acc lor m))
  in
  assert (rxc_m1_sum = rxc_m1_lor) ;
  let rx_mask = Array.fold rxc_masks ~init:0 ~f:(fun acc m -> acc lor m) in
  let n_conjunctions = Array.count modules ~f:is_conjunction in
  assert (n_conjunctions = 1 + 4 + 4) ;
  let is_rxc = Array.create ~len:n (-1) in
  (* we'll mutate the original input a bit *)
  for i = 0 to 3 do
    (* for quick access to be able to tell if it's an rxc node and which *)
    is_rxc.(rxc.(i).id) <- i ;
    (* remove rxb, rxa and rx from the system we don't need them anymore *)
    rxc.(i).out <- List.filter rxc.(i).out ~f:(fun j -> j <> rxb.(i).id)
  done ;
  {modules; is_rxc; rxc_masks; rx_mask}

let run_cycle {modules; is_rxc; rxc_masks; _} ff_state c_state ~f =
  let q = Queue.create () in
  let flip i =
    let x = not (Array.unsafe_get ff_state i) in
    Array.unsafe_set ff_state i x ;
    x
  in
  let conjunct src i v =
    let b = 1 lsl src in
    let j = Array.unsafe_get is_rxc i in
    let mask = Array.unsafe_get rxc_masks j in
    c_state := if v then !c_state lor b else !c_state land lnot b ;
    let v = !c_state land mask in
    (v <> mask, v)
  in
  List.iter modules.(0).out ~f:(fun j -> Queue.enqueue q (0, j, false)) ;
  while
    match Queue.dequeue q with
    | None -> false
    | Some (_, i, true) when (Array.unsafe_get modules i).is_ff -> true
    | Some (src, i, v) ->
        let m = Array.unsafe_get modules i in
        let x, c = if m.is_ff then (flip i, 0) else conjunct src i v in
        List.iter m.out ~f:(fun j -> Queue.enqueue q (i, j, x)) ;
        f m i x c ;
        true
  do
    ()
  done

module M = struct
  type t = system

  let parse = parse_and_validate

  let part1 s =
    (* 712543680 *)
    let n = Array.length s.modules in
    let ff_state, c_state = (Array.create ~len:n false, ref 0) in
    let n_lo, n_hi = (Array.create ~len:n 0, Array.create ~len:n 0) in
    let k_lo, k_hi = (ref 0, ref 0) in
    let on_pulse m i x _c =
      let count = if x then n_hi else n_lo in
      count.(i) <- count.(i) + 1 ;
      if not m.is_ff then (
        if x then incr k_lo else incr k_hi ;
        if !c_state land s.rx_mask = s.rx_mask then incr k_lo else incr k_hi
        )
    in
    for _ = 1 to 1000 do
      run_cycle s ff_state c_state ~f:on_pulse
    done ;
    let n_lo, n_hi =
      Array.foldi s.modules ~init:(0, 0) ~f:(fun i (lo, hi) m ->
          let n_out = List.length m.out + Bool.to_int (s.is_rxc.(i) >= 0) in
          (lo + (n_out * n_lo.(i)), hi + (n_out * n_hi.(i))) )
    in
    let n_lo = n_lo + (1000 * (1 + List.length s.modules.(0).out)) in
    let n_lo, n_hi = (n_lo + !k_lo, n_hi + !k_hi) in
    n_lo * n_hi |> Int.to_string

  let part2 s =
    (* 238920142622879 *)
    let n = Array.length s.modules in
    let ff_state, c_state = (Array.create ~len:n false, ref 0) in
    let cycle_len = Array.create ~len:4 0 in
    let iter = ref 1 in
    let on_pulse m i _ c =
      if not m.is_ff then
        let j = s.is_rxc.(i) in
        let mask = s.rxc_masks.(j) in
        if c land mask = mask && cycle_len.(j) = 0 then
          cycle_len.(j) <- !iter
    in
    while Array.exists cycle_len ~f:(( = ) 0) do
      run_cycle s ff_state c_state ~f:on_pulse ;
      incr iter
    done ;
    let rec gcd a b = if b = 0 then a else gcd b (a mod b) in
    let lcm a b = a * b / gcd a b in
    Array.fold cycle_len ~init:1 ~f:lcm |> Int.to_string
end

include M
include Day.Make (M)

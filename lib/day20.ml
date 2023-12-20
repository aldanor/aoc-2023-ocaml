open Core

type module_ =
  { mutable name: string
  ; mutable is_ff: bool
  ; mutable out: int list
  ; mutable inputs: int list }
[@@deriving show {with_path= false}]

type system = {modules: module_ array; n: int}
[@@deriving show {with_path= false}]

let system_of_string s =
  let module P = Imports.StreamParser in
  let p = P.create s in
  let modules =
    Array.init (26 * 26) ~f:(fun _ ->
        {name= ""; is_ff= false; out= []; inputs= []} )
  in
  let module_ids = Int.Table.create ~size:(26 * 26) () in
  Hashtbl.add_exn module_ids ~key:(-1) ~data:0 ;
  let parse_letter c = Char.to_int c - Char.to_int 'a' in
  let names = Array.create ~len:(Array.length modules) "" in
  names.(0) <- "broadcaster" ;
  let parse_module () =
    let name = ref (String.of_char (P.hd_u p)) in
    let i = ref (parse_letter (P.parse_char_u p)) in
    while P.not_eof p && P.is_hd_lowercase_u p do
      name := !name ^ String.of_char (P.hd_u p) ;
      i := (!i * 26) + parse_letter (P.parse_char_u p)
    done ;
    let i = !i in
    let index =
      Hashtbl.find_or_add module_ids i ~default:(fun () ->
          Hashtbl.length module_ids )
    in
    names.(index) <- !name ;
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
    modules.(i).name <- names.(i) ;
    List.iter out ~f:(fun j ->
        modules.(j).inputs <- i :: modules.(j).inputs ) ;
    P.skip p 1
  done ;
  {modules; n= Hashtbl.length module_ids}

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 712543680 *)
    let s = system_of_string s in
    let q = Queue.create ~capacity:100 () in
    let ff_state = Array.create ~len:s.n false in
    let c_state = Array.create ~len:s.n false in
    let n_hi = ref 0 in
    let n_lo = ref (1000 * (1 + List.length s.modules.(0).out)) in
    for _ = 1 to 1000 do
      List.iter s.modules.(0).out ~f:(fun j ->
          Queue.enqueue q (0, j, false) ) ;
      while
        match Queue.dequeue q with
        | None -> false
        | Some (_, i, v) when s.modules.(i).is_ff && v -> true
        | Some (src, i, v) ->
            let m = s.modules.(i) in
            let x =
              if m.is_ff then (
                let x = not ff_state.(i) in
                ff_state.(i) <- x ;
                x )
              else (
                c_state.(src) <- v ;
                not (List.for_all m.inputs ~f:(fun j -> c_state.(j))) )
            in
            List.iter m.out ~f:(fun j ->
                if x then incr n_hi else incr n_lo ;
                Queue.enqueue q (i, j, x) ) ;
            true
      do
        ()
      done
    done ;
    !n_hi * !n_lo |> Int.to_string

  let part2 _ = ""
end

include M
include Day.Make (M)

let%expect_test _ =
  "broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a"
  |> run_test ~part:1 ;
  [%expect {| 32000000 |}]

let%expect_test _ =
  "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output"
  |> run_test ~part:1 ;
  [%expect {| 11687500 |}]

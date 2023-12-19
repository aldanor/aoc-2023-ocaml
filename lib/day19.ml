open Core
open Imports

(* special cases: 0 is reject, 1 is accept, 2 is start *)
type workflow_id = int [@@deriving show {with_path= false}]

type operator = Less | Greater [@@deriving show {with_path= false}]

type condition = {dim: int; op: operator; value: int; wid: workflow_id}
[@@deriving show {with_path= false}]

type workflow =
  {mutable conditions: condition list; mutable otherwise: workflow_id}
[@@deriving show {with_path= false}]

type part = int array [@@deriving show {with_path= false}]

type problem =
  { workflows: workflow array
  ; n_workflows: int
  ; parts: int array
  ; n_parts: int }
[@@deriving show {with_path= false}]

let parse_inputs s ~parse_parts =
  let module P = StreamParser in
  let p = P.create s in
  let max_workflows = 512 in
  let workflow_ids = Int.Table.create ~size:max_workflows () in
  Hashtbl.add_exn workflow_ids ~key:0 ~data:0 ;
  Hashtbl.add_exn workflow_ids ~key:1 ~data:1 ;
  let parse_letter c = Char.to_int c - Char.to_int 'a' in
  Hashtbl.add_exn workflow_ids
    ~key:((26 * parse_letter 'i') + parse_letter 'n')
    ~data:2 ;
  let workflows =
    Array.init max_workflows ~f:(fun _ -> {conditions= []; otherwise= 0})
  in
  workflows.(1).otherwise <- 1 ;
  let parse_workflow_id () =
    match P.parse_char_u p with
    | 'R' -> 0
    | 'A' -> 1
    | c ->
        let c = ref (parse_letter c) in
        c := (26 * !c) + parse_letter (P.parse_char_u p) ;
        if P.is_hd_lowercase_u p then
          c := (26 * !c) + parse_letter (P.parse_char_u p) ;
        Hashtbl.find_or_add workflow_ids !c ~default:(fun () ->
            Hashtbl.length workflow_ids )
  in
  let parse_dim () =
    match P.parse_char_u p with
    | 'x' -> 0
    | 'm' -> 1
    | 'a' -> 2
    | 's' -> 3
    | _ -> assert false
  in
  let parse_op () =
    match P.parse_char_u p with
    | '<' -> Less
    | '>' -> Greater
    | _ -> assert false
  in
  let rec fold_conditions conditions otherwise =
    (* remove redundant conditions from the end (it's already reversed) *)
    match conditions with
    | c :: rest when c.wid = otherwise -> fold_conditions rest otherwise
    | _ -> conditions
  in
  let parse_workflow () =
    let wid = parse_workflow_id () in
    P.skip p 1 ;
    let conditions = ref [] in
    while P.snd_equals_u p '<' || P.snd_equals_u p '>' do
      let dim = parse_dim () in
      let op = parse_op () in
      let value = P.parse_int p in
      let dst = parse_workflow_id () in
      conditions := {dim; op; value; wid= dst} :: !conditions ;
      P.skip p 1
    done ;
    let otherwise = parse_workflow_id () in
    let conditions = List.rev (fold_conditions !conditions otherwise) in
    let workflow = {conditions; otherwise} in
    workflows.(wid) <- workflow ;
    P.skip p 2
  in
  while not (P.hd_equals_u p '\n') do
    parse_workflow ()
  done ;
  let max_parts = 512 in
  let parts = Array.create ~len:(max_parts * 4) 0 in
  let n_parts, i = (ref 0, ref 0) in
  if parse_parts then
    while P.not_eof p do
      P.skip p 2 ;
      for _ = 0 to 3 do
        P.skip p 2 ;
        parts.(!i) <- P.parse_int p ;
        incr i
      done ;
      incr n_parts
    done ;
  { workflows
  ; n_workflows= Hashtbl.length workflow_ids
  ; parts
  ; n_parts= !n_parts }

type interval = {lo: int; hi: int}

let pp_interval ppf {lo; hi} = Format.fprintf ppf "%d..=%d" lo hi

let show_interval {lo; hi} = sprintf "%d..=%d" lo hi

type box = {x: interval; m: interval; a: interval; s: interval}
[@@deriving show {with_path= false}]

let volume {x; m; a; s} =
  (x.hi - x.lo + 1)
  * (m.hi - m.lo + 1)
  * (a.hi - a.lo + 1)
  * (s.hi - s.lo + 1)

let split_interval iv op value =
  let {lo; hi}, yes, no = (iv, ref None, ref None) in
  ( match op with
  | Less ->
      if hi < value then yes := Some iv
      else if lo < value then yes := Some {lo; hi= value - 1} ;
      if lo >= value then no := Some iv
      else if hi >= value then no := Some {lo= value; hi}
  | Greater ->
      if lo > value then yes := Some iv
      else if hi > value then yes := Some {lo= value + 1; hi} ;
      if hi <= value then no := Some iv
      else if lo <= value then no := Some {lo; hi= value} ) ;
  (!yes, !no)

let split_box box dim op value =
  let iv, f =
    match dim with
    | 0 -> (box.x, fun x -> {box with x})
    | 1 -> (box.m, fun m -> {box with m})
    | 2 -> (box.a, fun a -> {box with a})
    | _ -> (box.s, fun s -> {box with s})
  in
  let yes, no = split_interval iv op value in
  (Option.map ~f yes, Option.map ~f no)

module M = struct
  type t = string

  let parse s = s

  let part1 s =
    (* 368523 *)
    let p = parse_inputs s ~parse_parts:true in
    let part_score o =
      p.parts.(o + 0) + p.parts.(o + 1) + p.parts.(o + 2) + p.parts.(o + 3)
    in
    let rec eval_part_wid' offset conditions otherwise =
      match conditions with
      | [] -> otherwise
      | {dim; op; value; wid} :: rest ->
          let pass =
            match op with
            | Less -> p.parts.(offset + dim) < value
            | Greater -> p.parts.(offset + dim) > value
          in
          if pass then wid else eval_part_wid' offset rest otherwise
    in
    let rec eval_part' offset wid =
      let workflow = p.workflows.(wid) in
      let wid =
        eval_part_wid' offset workflow.conditions workflow.otherwise
      in
      match wid with
      | 0 -> 0
      | 1 -> part_score offset
      | wid -> eval_part' offset wid
    in
    let eval_part part_id = eval_part' (part_id lsl 2) 2 in
    let score = ref 0 in
    for part_id = 0 to p.n_parts - 1 do
      score := !score + eval_part part_id
    done ;
    !score |> Int.to_string

  let part2 s =
    (* 124167549767307 *)
    let p = parse_inputs s ~parse_parts:false in
    let ans = ref 0 in
    let rec f queue =
      match queue with
      | [] -> ()
      | (_, [], 0) :: queue -> f queue
      | (box, [], 1) :: queue ->
          ans := !ans + volume box ;
          f queue
      | (box, [], wid) :: queue ->
          let w = p.workflows.(wid) in
          f ((box, w.conditions, w.otherwise) :: queue)
      | (box, {dim; op; value; wid} :: rest, otherwise) :: queue ->
          let yes, no = split_box box dim op value in
          let queue =
            match yes with
            | None -> queue
            | Some yes -> (yes, [], wid) :: queue
          in
          let queue =
            match no with
            | None -> queue
            | Some no -> (no, rest, otherwise) :: queue
          in
          f queue
    in
    let iv_start = {lo= 1; hi= 4000} in
    let box_start = {x= iv_start; m= iv_start; a= iv_start; s= iv_start} in
    let w_start = p.workflows.(2) in
    f [(box_start, w_start.conditions, w_start.otherwise)] ;
    !ans |> Int.to_string
end

include M
include Day.Make (M)

let%expect_test _ =
  "px{a<2006:qkq,m>2090:A,rfg}\n\
   pv{a>1716:R,A}\n\
   lnx{m>1548:A,A}\n\
   rfg{s<537:gd,x>2440:R,A}\n\
   qs{s>3448:A,lnx}\n\
   qkq{x<1416:A,crn}\n\
   crn{x>2662:A,R}\n\
   in{s<1351:px,qqz}\n\
   qqz{s>2770:qs,m<1801:hdj,R}\n\
   gd{a>3333:R,R}\n\
   hdj{m>838:A,pv}\n\n\
   {x=787,m=2655,a=1222,s=2876}\n\
   {x=1679,m=44,a=2067,s=496}\n\
   {x=2036,m=264,a=79,s=2244}\n\
   {x=2461,m=1339,a=466,s=291}\n\
   {x=2127,m=1623,a=2188,s=1013}" |> run_test ;
  [%expect {| 19114 167409079868000 |}]

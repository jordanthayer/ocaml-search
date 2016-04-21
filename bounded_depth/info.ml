(* $Id: info.ml,v 1.1 2005/03/16 00:41:42 ruml Exp ruml $

   maintaining search stats and parameters
*)


(******** limiting search time or effort ********)


type search_limit =
  | Never
  | Generated of int
  | Expanded of int
  | Leaves of int
  | Branches of int
  | Time of float

let split_limit lim num =
  match lim with
    | Never -> Array.create num Never
    | Generated n -> (let v = n / num in
			Array.init num (fun i -> Generated (i * v)))
    | Expanded n -> (let v = n / num in
		       Array.init num (fun i -> Expanded (i * v)))
    | Leaves n -> (let v = n / num in
		     Array.init num (fun i -> Leaves (i * v)))
    | Branches n -> (let v = n / num in
		       Array.init num (fun i -> Branches (i * v)))
    | Time n -> (let v = n /. (float num) in
		   Array.init num (fun i -> Time v))


let limit_from_string s =
  (** from a general-purpose string *)
  let s = String.lowercase s in
    if s = "never"
    then Never
    else if s = "first"
    then Leaves 1
    else
      try
	if String.contains s '.'
	then Time (float_of_string s)
	else Expanded (int_of_string s)
      with Failure _ -> Wrutils.nicefailwith ("don't understand limit: " ^ s)


let string_from_limit = function
    (** to a general-purpose string *)
  | Never -> "never"
  | Expanded n -> string_of_int n
  | Generated n -> string_of_int n
  | Leaves 1 -> "first"
  | Time t -> string_of_float t
  | Leaves _ | Branches _ -> failwith "no string representation of this limit"


let rec output_limits_to_dfs outchan = function
  | Expanded n :: ls ->
      Datafile.write_pairs outchan
	[ "nodes expanded limit", string_of_int n; ];
      output_limits_to_dfs outchan ls
  | Generated n :: ls ->
      Datafile.write_pairs outchan
	[ "nodes generated limit", string_of_int n; ];
      output_limits_to_dfs outchan ls
  | Leaves n :: ls ->
      Datafile.write_pairs outchan
	[ "leaves limit", string_of_int n; ];
      output_limits_to_dfs outchan ls
  | Time n :: ls ->
      Datafile.write_pairs outchan
	[ "time limit", string_of_float n; ];
      output_limits_to_dfs outchan ls
  | Branches n :: ls ->
      Datafile.write_pairs outchan
	[ "branch limit", string_of_int n; ];
      output_limits_to_dfs outchan ls
  | Never :: ls ->
      output_limits_to_dfs outchan ls
  | [] -> ()


let arg_string_from_limit = function
    (** to one of several special-purpose command-line switches *)
  | Never -> "--never"
  | Expanded n -> (Wrutils.str "--exp %d" n)
  | Generated n -> (Wrutils.str "--gen %d" n)
  | Leaves n -> (Wrutils.str "--leaves %d" n)
  | Branches n -> (Wrutils.str "--branches %d" n)
  | Time n -> (Wrutils.str "--time %f" n)


let to_arg_string limits =
  List.fold_left (fun accum current ->
		    accum ^ " " ^ (arg_string_from_limit current)) "" limits

let to_string limit_list =
  match limit_list with
    | [] -> ""
    | _ ->
	let str =
	  List.fold_left (fun accum l ->
			    accum ^ " " ^ string_from_limit l) "" limit_list in
	  String.sub str 1 ((String.length str) - 2)

(************ recording search parameters and stats *********)


(* want to capture common idioms in search algorithms and prevent having to
   pass around lots of arguments.  also keeps the search statistics in one
   place.

   probably not worth making an object because it wouldn't get subclassed
   much.
*)


(*  what should API be for generating children?

    separate function for binary/non-binary : yuck
    optional return : allocate Some
    function to get num_children : extra call, extra arg.  but allocation
    is a call itself so perhaps this option is best.
*)


type ('node, 'saved) info = {
  (** type parameterized by search node and saved search node types *)
  (* nodes generated includes branches, leaves, and prunes *)
  mutable nodes : int;
  mutable leaves : int;
  mutable branches : int;
  mutable prunes : int;
  mutable best_so_far : 'saved option;
  leaf_p : 'node -> bool;
  (* true if new is better than saved *)
  better_p : 'node -> 'saved -> bool;
  copy_state : 'node -> 'saved;
  log : ('node, 'saved) info -> 'node -> unit;
  optimal_p : 'saved -> bool;
  prune_p : 'saved -> 'node -> bool;
  mutable halt_p : ('node, 'saved) info -> bool;
  num_children : 'node -> int;
  get_child : 'node -> int -> 'node;
}


(********** construction *********)


let rec make_halt_p ?(accum = (fun _ -> false)) halts =
  match halts with
    | [] -> accum
    | hd::tl ->
	let nf =
	  match hd with
	    | Never -> (fun _ -> false)
	    | Expanded max -> (fun i -> (i.nodes >= max))
	    | Generated max -> (fun i -> (i.nodes >= max))
	    | Leaves max -> (fun i -> (i.leaves >= max))
	    | Branches max -> (fun i -> (i.branches >= max))
	    | Time duration ->
		let limit = duration +. Sys.time () in
		  (fun i ->
		     Sys.time () >= limit) in
	  make_halt_p ~accum:(fun i -> (accum i) || (nf i)) tl


let make num_children get_child leaf_p better_p optimal_p copy_state
  prune_p log prev_best halt_spec =
  { nodes = 0 ;
    leaves = 0 ;
    branches = 0 ;
    prunes = 0 ;
    best_so_far = prev_best ;
    leaf_p = leaf_p;
    better_p = better_p;
    copy_state = copy_state;
    log = log;
    optimal_p = optimal_p;
    prune_p = prune_p;
    halt_p = make_halt_p halt_spec;
    num_children = num_children;
    get_child = get_child;
  }


(********** using it *********)

let get_nodes i = i.nodes

let get_halt i = i.halt_p

(* assign new halt clause *)
let set_halt_or i spec f =
  let n = make_halt_p spec in
    i.halt_p <- (fun info -> (n info) || (f info))

let optimal_so_far i =
  match i.best_so_far with
    None -> false
  | Some sol -> i.optimal_p sol


let leaf_p i n =
  if i.leaf_p n then
    (i.leaves <- i.leaves + 1;
     true)
  else
    false


let optimal_p i n =
  (** calls logger iff new best *)
  if (match i.best_so_far with
	None -> true
      | Some sol -> i.better_p n sol) then
    let new_best = i.copy_state n in
      i.best_so_far <- Some new_best;
      i.log i n;
      i.optimal_p new_best
  else
    false


let best_sol i =
  (** useful in a logging function *)
  match i.best_so_far with
    None -> failwith "no sol yet!"
  | Some s -> s


let prune_p i n =
  if (match i.best_so_far with
	None -> false
      | Some s -> i.prune_p s n) then
    (i.prunes <- i.prunes + 1;
     true)
  else
    false


let halt_p i =
  i.halt_p i


let incr_branches i =
  i.branches <- i.branches + 1


let num_children i n =
  (** only needed by those functions for whom num_children is not easily
    accessible *)
  i.num_children n


let get_child i n index =
  i.nodes <- i.nodes + 1;
  i.get_child n index


let leaf_or_prune_p i n =
  (i.leaf_p n) ||
  (match i.best_so_far with
     None -> false
   | Some sol -> i.prune_p sol n)

let check_best_and_optimal i n =
  (* also calls logger if the solution is better than the previous! *)
  if (match i.best_so_far with
	  None -> true
	| Some sol -> i.better_p n sol)
  then begin
    i.best_so_far <- Some (i.copy_state n);
    i.log i n;
    i.optimal_p (match i.best_so_far with
                     None -> failwith "impossible"
		   | Some s -> s)
  end else false

(********** logging *********)

(* note that logger is only called when a new best is encountered *)


let curr_best i =
  i.best_so_far

let stats i =
  i.nodes, i.leaves, i.branches, i.prunes


let null_logger info curr =
  ()


let extended_cost_logger ?(silent = false) cost_hd get_cost ch =
  if not silent
  then
    (Datafile.write_colnames ch (cost_hd @ ["nodes"; "leaves"; "raw cpu time"]);
     List.iter (fun _ -> (Verb.pr Verb.always "%f\t" infinity)) cost_hd;
     Verb.pr Verb.always "%d\t%d\t%f\n" 0 0 0.);
  let start = Sys.time () in
    (fun i _ ->
       Verb.pf Verb.always ch "%s\t%1d\t%1d\t%e\n"
	 (get_cost (best_sol i))
	 i.nodes i.leaves (Sys.time () -. start);
       flush ch)


let basic_logger get_cost ch =
  Datafile.write_colnames ch
    ["nodes"; "leaves"; "best sol cost"; "raw cpu time"];
  let start = Sys.time () in
    (fun i _ ->
       Verb.pf Verb.always ch "%1d\t%1d\t%e\t%e\n"
       i.nodes i.leaves (get_cost (best_sol i)) (Sys.time () -. start);
       flush ch)


let make_log_sampled_logger f =
  (* see lisp/search/basic_tree.lisp for debugging code.

     first 175 tries succeed, then gets sparser exponentially. plateaus at
     78 additional lines for 10x tries *)
  let factor = 1.03
  and line_count = ref 0.
  and next_logged = ref 1. in
    fun i n ->
      line_count := !line_count +. 1.;
      if (!line_count >= !next_logged)
      then (next_logged := !next_logged *. factor;
	    f i n)


(********** reporting *********)


let print_results ch s o c =
  (** takes stats, optimal, complete *)
  let nodes, leaves, branches, prunes = s in
    Verb.pf Verb.always ch "%d nodes generated\n" nodes ;
    Verb.pf Verb.always ch "%d leaves visited\n" leaves ;
    Verb.pf Verb.always ch "%d nodes pruned\n" prunes ;
    Verb.pf Verb.always ch "%d branching nodes visited\n" branches ;
    Verb.pf Verb.always ch "The search tree was%s exhausted.\n"
      (if c then "" else " not") ;
    Verb.pf Verb.always ch "This solution is%s optimal.\n"
      (if (o || c) then "" else " not necessarily")


let append_incomplete (n, s, o) =
  (** for drop-in replace of potentially complete methods by
    incomplete methods *)
  n, s, o, false


(* EOF *)

(* $Id: limit.ml,v 1.1 2006/11/18 01:27:16 ruml Exp ruml $
   specifying time limits
*)


(******** limiting search time or effort ********)


type t = | Never
	 | Generated of int
	 | Expanded of int
	 | Time of float
	 | WallTime of float
	 | Memory of float		(* in KB *)
	 | MachineMemory

(* Currently WallTime relies on Unix.time(), which has second resolution.
   We may want to consider switching to Unix.timeofday(), which is a little
   better on that front.  The documentation doesn't specify what the difference
   is, so we'll need to know that before switching over. *)


let rec to_string ?(accum = "") lims =
  match lims with
      [] -> accum
    | hd::tl ->
	(match hd with
	   | Never -> to_string ~accum:accum  tl
	   | Memory m ->
	       to_string ~accum:(Wrutils.str "%s --kb %f" accum m)  tl
	   | Generated g ->
	       to_string ~accum:(Wrutils.str "%s --gen %i" accum g) tl
	   | Expanded e ->
	       to_string ~accum:(Wrutils.str "%s --exp %i" accum e) tl
	   | Time t ->
	       to_string ~accum:(Wrutils.str "%s --time %f" accum t) tl
	   | WallTime t ->
	       to_string ~accum:(Wrutils.str "%s --wtime %f" accum t) tl
	   | MachineMemory ->
	       to_string ~accum:(Wrutils.str "%s --memory max" accum)  tl
	)

let rec to_trailpairs lims =
  (** Converts a list of limits [lims] into a set of trail pair strings *)
  match lims with
      [] -> []
    | hd::tl ->
	(match hd with
	   | Never -> to_trailpairs tl
	   | Memory m -> ("memory limit",
			  (string_of_float m))::(to_trailpairs tl)
	   | Generated g -> ("nodes generated limit",
			     (string_of_int g))::(to_trailpairs tl)
	   | Expanded e -> ("nodes expanded limit",
			    (string_of_int e))::(to_trailpairs tl)
	   | Time t ->  ("time limit", (string_of_float t))::(to_trailpairs tl)
	   | WallTime t ->  ("wall time limit",
			     (string_of_float t))::(to_trailpairs tl)
	   | MachineMemory -> ("memory limit","max")::(to_trailpairs tl)
	)


type 'node return =
  | Nothing
  | Incumbent of float * 'node (* the float is the solution quality *)


let limit_from_string s =
  (** Builds a limit.t halting predicate from [s], a general-purpose string *)
  let s = String.lowercase s in
    if s = "never"
    then Never
    else
      try
	if String.contains s '.' then
	  Time (float_of_string s)
	else if ((String.compare s "max") = 0) then
	  MachineMemory
	else
	  Expanded (int_of_string s)
      with Failure _ -> Wrutils.nicefailwith ("don't understand limit: " ^ s)


let string_from_limit = function
    (** Converts a [limit.t] to a general-purpose string *)
    Never -> "never"
  | Memory m -> string_of_float m
  | Generated _ -> failwith "no string representation of this limit"
  | Expanded n -> string_of_int n
  | Time t -> string_of_float t
  | WallTime t -> string_of_float t
  | MachineMemory -> "max"

let arg_specs limit =
  (** [arg_specs limit] gets a list of argument specs (as used by
      Arg.parse) that will handle argument parsing of limits by adding
      limits to the [limit] list ref. *)
  [ "--memory",
    Arg.String
      (fun s -> match s with
	 | "max" -> limit :=
	     (Memory (Memtest.get_size_main_memory ())) :: !limit
	 | m -> (
	     try
	       let words = float_of_string m in
	       let bytes = words *. (float (Sys.word_size / 8)) in
	       let kb = bytes /. 1000. in
		 limit := (Memory kb) :: !limit
	     with Failure "float_of_string" ->
	       failwith (Wrutils.str "error converting %s to float" m))),
    ("max makes the search stop when main memory is full." ^
       " Number makes the search stop when the major heap exceeds" ^
       " the specified size.");
    "--kb",
    Arg.Float (fun kb -> limit := (Memory kb) :: !limit),
    "The maximum number of kilobytes in the the heap";

    "--mb",
    Arg.Float (fun mb -> limit := (Memory (mb *. 1000.)) :: !limit),
    "The maximum number of megabytes in the the heap";

    "--gb",
    Arg.Float (fun gb -> limit := (Memory (gb *. 1000000.)) :: !limit),
    "The maximum number of gigabytes in the the heap";

    "--wall", Arg.Float
      (fun l -> limit := (WallTime l)::!limit),
    "wall time limit (in seconds, default no limit)";

    "--time", Arg.Float (fun l -> limit := (Time l)::!limit),
    "search time limit (in seconds, default no limit)";

    "--gen", Arg.Int (fun l -> limit := (Generated l)::!limit),
    "search generation limit";

    "--exp", Arg.Int (fun l -> limit := (Expanded l)::!limit),
    "search expansion limit";
  ]

let usage_string =
  "[--memory (max|<words>)] [--kb <kilobytes>] [--mb <megabytes>] "
  ^ "[--gb <gigabytes>] [--wall <wall time>] [--time <CPU time>] "
  ^ "[--gen <generations>] [--exp <expansions>]"


(************ recording search parameters and stats *********)


(* want to capture common idioms in search algorithms and prevent having to
   pass around lots of arguments.  also keeps the search statistics in one
   place.

   probably not worth making an object because it wouldn't get subclassed
   much.
*)


type 'node info = {
  mutable expanded : int;
  mutable generated : int;
  mutable pruned : int;
  mutable max_q : int;
  mutable duplicates : int;
  mutable incumbent : 'node return;
  mutable start_time : float;
  mutable halt_p : 'node info -> bool;
  better_p : 'node -> 'node -> bool;
  mutable log: 'node info -> unit;
}


let unwrap_info get_data i =
  {expanded = i.expanded;
   generated = i.generated;
   pruned = i.pruned;
   max_q = i.max_q;
   start_time = i.start_time;
   duplicates = i.duplicates;
   incumbent = (match i.incumbent with
			  Nothing -> Nothing
			| Incumbent (v,node) -> Incumbent (v,get_data node));
   halt_p = (fun _ -> false);  (* these may rely on the node data struct *)
   better_p = (fun _ _ -> false); (* and as a result, cannot be unwrapped*)
   log = (fun _ -> ());}


let rec make_halt_p ?(accum = (fun _ -> false)) limit =
  (** Builds a halt out of several different types of limit.
      Uses an ormap across all of the halts specified in [limit] *)
  match limit with
      [] -> accum
    | hd::tl ->
	let nf =
	  match hd with
	      Never -> (fun i -> false)
	    | MachineMemory ->
		let b = (Memtest.get_size_main_memory ()) *. 1000. in
		let initial_gcs = Gc.quick_stat () in
		let initial_hs = initial_gcs.Gc.heap_words * 8 in

		let _complained = ref false in
		  (fun i ->
		     let gcs = Gc.quick_stat () in
		     let hs = gcs.Gc.heap_words * 8 in
		     let should_stop =
		       (hs - initial_hs) >= (int_of_float b) in
		       if(should_stop) then
			 (
			 (*
			   if(not !complained) then
			     (
			       complained := true;
			       Datafile.write_pairs stdout
				 ["Out of Memory",
				  Printf.sprintf "true mem cap %f heap %d"
				    b hs
				 ];
			     );
			 *)
			   true;
			 )
		       else
			 false
		  )
	    | Memory kb ->
		let bytes = kb *. 1000. in
		let words = truncate (bytes /. ((float Sys.word_size) /. 8.)) in
		  (fun i ->
		     let gcs = Gc.quick_stat () in
		     let hs = gcs.Gc.heap_words in
		       hs >= words)
	    | Generated max -> (fun i -> (i.generated >= max))
	    | Expanded max -> (fun i -> (i.expanded >= max))
	    | Time duration -> (fun i ->
				  Sys.time () >= (i.start_time +. duration))
	    | WallTime duration -> (let limit = duration +. Unix.time () in
				      (fun i ->
					 Unix.time () >= limit))
	in make_halt_p ~accum:(fun i -> (accum i) || (nf i)) tl


let make_default_logger ?(silent = false) ?(ch = stdout) ?(time = (Sys.time()))
    get_cost get_length =
  (** Standard log output for duplicate domains seems to be
      cost length expanded generated time other domains do not use solution
      length in terms of actions.  Have it passed in on a per domain
      basis until it can be standardized. [get_cost] and [get_length] should
      be able to take a node data structure and return the cost / length of
      the solution represented by the nope. *)
      if not silent
      then
	(Datafile.write_colnames ch
	   ["sol cost"; "sol length" ;"nodes expanded";
	    "nodes generated"; "duplicates encountered"; "raw cpu time"];
	 Verb.pf Verb.always ch "%f\t%d\t%d\t%d\t%d\t%f\n" infinity 0 0 0 0 0.);
      let start = time in
	(fun i ->
	   let t = (Sys.time ()) -. start in
	     match i.incumbent with
		 Nothing ->
		   Verb.pf Verb.always ch "0.\t0\t%d\t%d\t%d\t%f\n"
		     i.expanded i.generated i.duplicates t
	       | Incumbent (q,inc) ->
		   Verb.pf Verb.always ch "%f\t%d\t%d\t%d\t%d\t%f\n"
		     (get_cost inc) (get_length inc) i.expanded i.generated
		     i.duplicates t)

let has_incumbent i =
  match i.incumbent with
      Nothing -> false
    | Incumbent (_,_) -> true


let make incumbent halt_spec better_p log =
  (** Creates a limit.info [incumbent] is an incumbent solution
      [halt_spec] is a list of the halting predicates to be used
      [better_p] is needed to determine if a new solution is better than
                 the current incumbent
      [log] logs new solutions as they become the incumbent *)
  { generated = 0;
    expanded = 0;
    duplicates = 0;
    pruned = 0;
    max_q = 1;
    start_time = Sys.time ();
    incumbent = incumbent;
    halt_p = make_halt_p halt_spec;
    better_p = better_p;
    log = log;}


let new_incumbent i x =
  (** [x] is a new solution.  If [x] is better that the current incumbent
      according to [i].better_p, then x will become the new incumbent
      solution *)
  let q,new_inc = (match x with
		     | Incumbent (q,inc) -> q,inc
		     | Nothing -> failwith "X isn't an incumbent?") in
    if (match i.incumbent with
	  | Nothing -> true
	  | Incumbent (_, inc) ->
	      Verb.pe Verb.debug "Quality: %f\n" q;
	      (not (i.better_p inc new_inc)))
    then (i.incumbent <- x;
	  i.log i)


let halt_p i =
  (** Should the current search recorded in [i] halt? *)
  i.halt_p i


let incr_exp i =
  (** increments the expansion count in [i] *)
  i.expanded <- i.expanded + 1

let incr_exp_n i n =
  (** increments the expansion count in [i] *)
  i.expanded <- i.expanded + n

let incr_gen i =
  (** increments the generation count in [i] *)
  i.generated <- i.generated + 1

let incr_gen_n i n =
  (** increases the generation count of [i] by [n] *)
  i.generated <- i.generated + n

let incr_prune i =
  (** increase the prune count of [i] *)
  i.pruned <- i.pruned + 1

let incr_prune_n i n =
  (** increase the prune count of [i] *)
  i.pruned <- i.pruned + n

let incr_dups i =
  (** increases the duplicate count of [i] *)
  i.duplicates <- i.duplicates + 1

let incr_dups_n i n=
  (** increases the duplicate count of [i] *)
  i.duplicates <- i.duplicates + n

let promising_p i n =
  (** Determines if the node [n] is promising in light of the current
      incumbent solution in [i] by using better_p *)
  match i.incumbent with
      Nothing -> true
	(* n strictly better than incumbent *)
    | Incumbent (quality,prev) -> not (i.better_p prev n)


let curr_q i l =
  (** Records the size [l] of the current queue, assuming that [l] is larger
      than previously recorded values *)
  if l > i.max_q then i.max_q <- l


let results5 i =
  (** Takes the node info structure [i] and returns an ordered 5-ple of
      the incumbent solution, number of nodes expanded, number of nodes
      generated, the number of nodes pruned, and the largest queue size. *)
  i.incumbent, i.expanded, i.generated, i.pruned, i.max_q

let unwrap_sol5 unwrap (s, x2, x3, x4, x5) =
  (** applies the function [unwrap] to the solution portion of a 5-ple
      generated by [results5] *)
  (unwrap s), x2, x3, x4, x5


let results6 i =
  (** Takes the node info structure [i] and returns an ordered 6-ple of
      the incumbent solution, number of nodes expanded, number of nodes
      generated, the number of nodes pruned, the largest queue size,
      and the number of duplicate nodes encountered. *)
  i.incumbent, i.expanded, i.generated, i.pruned, i.max_q, i.duplicates

let unwrap_sol6 unwrap (s, x2, x3, x4, x5, x6) =
  (** applies the function [unwrap] to the solution portion of a 6-ple
      generated by [results6] *)
  (unwrap s), x2, x3, x4, x5, x6

let unwrap_sol5_into_6 unwrap (s, x2, x3, x4, x5) =
  (** applies the function [unwrap] to the solution portion of a 5-ple
      generated by [results5] *)
  (unwrap s), x2, x3, x4, x5, 0

let unwrap_sol6_into_5 unwrap (s, x2, x3, x4, x5, x6) =
  (** applies the function [unwrap] to the solution portion of a 6-ple
      generated by [results6] *)
  (unwrap s), x2, x3, x4, x5

let has_limit df limit =
  try
    match limit with
      | Never -> true
      | Generated g ->
	  (let gen_lim = int_of_string
	     (Datafile.get_val df "nodes generated limit") in
	     g <= gen_lim)
      | Expanded e ->
	  (let exp_lim = int_of_string
	     (Datafile.get_val df "nodes expanded limit") in
	     e <= exp_lim)
      | Time t ->
	  (let time_lim = float_of_string
	     (Datafile.get_val df "time limit") in
	     t <= time_lim)
      | WallTime t ->
	  (let time_lim = float_of_string
	     (Datafile.get_val df "wall time limit") in
	     t <= time_lim)
      | Memory m ->
	  (let mem_lim = float_of_string
	     (Datafile.get_val df "memory limit") in
	     m <= mem_lim)
      | MachineMemory _ ->
	  failwith "What is machine memory... please add it here"
  with Failure _ -> true


let has_new_limits filename limits =
  try
    let df = Datafile.load filename in
    let rec check_lim lim =
      match lim with
	  [] -> false
	| hd :: tl -> (if has_limit df hd
		       then check_lim tl
		       else true) in
      check_lim limits
  with _ -> true (*On error loading df?*)



(***** Dead Code ******)

(*

let basic_logger get_cost ch =
  Datafile.write_colnames ch
    ["nodes"; "leaves"; "best sol cost"; "raw cpu time"];
  let start = Sys.time () in
    (fun i _ ->
       Printf.fprintf ch "%1d\t%1d\t%e\t%e\n"
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
    Printf.fprintf ch "%d nodes generated\n" nodes ;
    Printf.fprintf ch "%d leaves visited\n" leaves ;
    Printf.fprintf ch "%d nodes pruned\n" prunes ;
    Printf.fprintf ch "%d branching nodes visited\n" branches ;
    Printf.fprintf ch "The search tree was%s exhausted.\n"
      (if c then "" else " not") ;
    Printf.fprintf ch "This solution is%s optimal.\n"
      (if (o || c) then "" else " not necessarily")


let append_incomplete (n, s, o) =
  (** for drop-in replace of potentially complete methods by
    incomplete methods *)
  n, s, o, false
*)


(* EOF *)

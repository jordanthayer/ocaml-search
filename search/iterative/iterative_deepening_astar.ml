(** Iterative deepening search Jordan Thayer - July 2009 *)

type 'a node = {
  data : 'a;
  g : float;
  h : float;
  f : float;
}


let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let better_p a b =
  (** Sorts nodes solely on total cost information *)
  a.g <= b.g


let make_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (c, g) -> { data = c;
			       g = g;
			       h = h c;
			       f = g +. (h c)}) (expand n.data n.g))


(************************************************************)
(* Simple bound models.                                     *)
(************************************************************)

let expansions = ref 0
let iter_no = ref 0
let this_bound = ref 0.
and next = ref infinity

let reset_bound_default ?(quiet = false) vl =
  if not quiet then Iterative_deepening.output_col_hdr ();
  iter_no := 0;
  expansions := 0;
  this_bound := vl;
  next := infinity

let see_expansion_default depth n children =
  incr expansions

let iteration_complete_default ?(quiet = false) _ _ =
  if not quiet
  then Iterative_deepening.output_row !iter_no !this_bound !expansions;
  incr iter_no;
  expansions := 0;
  this_bound := !next;
  next := infinity


let iteration_complete_double _ _ =
  (* a next bound function that doubles the current bound. *)
  Iterative_deepening.output_row !iter_no !this_bound !expansions;
  incr iter_no;
  expansions := 0;
  this_bound := (!this_bound *. 2.);
  next := infinity

let check_bound_default n =
  if n.f > !this_bound && n.f < !next then next := n.f;
  n.f <= !this_bound

(************************************************************)
(* Functions that accept the model as their arguments.      *)
(************************************************************)

let no_dups_with_model id_fun
    sface reset_bound see_expansion iteration_complete check_bound =
  (** Performs an IDA* search from the initial state to a goal, for
      domains with no duplicates.  Accepts bound estimation
      functions. *)
  let search_interface = Search_interface.make
    ~node_expand:(make_expand
		    sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      h = 0.;
      f = 0.;}
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    reset_bound (sface.Search_interface.h sface.Search_interface.initial);
    Limit.unwrap_sol5 unwrap_sol
      (id_fun search_interface better_p see_expansion check_bound
	 iteration_complete)


let dups_with_domain_model id_fun
    sface reset_bound see_expansion iteration_complete check_bound =
  (** Performs an IDA* search from the initial state to a goal, for
      domains with no duplicates.

      Accepts bound estimation functions that operate on states from
      the domain. *)
  let see_expansion depth n cs =
    see_expansion
      depth
      (n.data, n.g, n.h)
      (List.map (fun c -> c.data, c.g, c.h) cs)
  and check_bound n =
    check_bound (n.data, n.g, n.h)
  in

  let search_interface = Search_interface.make
    ~node_expand:(make_expand
		    sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      h = sface.Search_interface.h sface.Search_interface.initial;
      f = sface.Search_interface.h sface.Search_interface.initial;}
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))
  in
    reset_bound (sface.Search_interface.h sface.Search_interface.initial);
    Limit.unwrap_sol6 unwrap_sol
      (id_fun search_interface better_p see_expansion check_bound
	 iteration_complete)

let with_domain_model id_fun
    sface reset_bound see_expansion iteration_complete check_bound =
  (** Performs an IDA* search from the initial state to a goal, for
      domains with no duplicates.

      Accepts bound estimation functions that operate on states from
      the domain. *)
  let see_expansion depth n cs =
    see_expansion
      (depth, n.data, n.g, n.h)
      (List.map (fun c -> c.data, c.g, c.h) cs)
  and check_bound n =
    check_bound (n.data, n.g, n.h)
  in

  let search_interface = Search_interface.make
    ~node_expand:(make_expand
		    sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      h = sface.Search_interface.h sface.Search_interface.initial;
      f = sface.Search_interface.h sface.Search_interface.initial;}
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))
  in
    reset_bound (sface.Search_interface.h sface.Search_interface.initial);
    Limit.unwrap_sol5 unwrap_sol
      (id_fun search_interface better_p see_expansion check_bound
	 iteration_complete)

let dups_with_model id_fun
    sface reset_bound see_expansion iteration_complete check_bound =
  (** Performs an IDA* search from the initial state to a goal, for
      domains with no duplicates.

      Accepts bound estimation functions that operate on
      Iterative_deepening_astar.nodes. *)
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand
		      sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~key:(wrap sface.Search_interface.key)
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	g = 0.;
	h = sface.Search_interface.h sface.Search_interface.initial;
	f = sface.Search_interface.h sface.Search_interface.initial;}
      better_p
      (Limit.make_default_logger (fun n -> n.f)
	 (wrap sface.Search_interface.get_sol_length))
  in
    reset_bound
      (sface.Search_interface.h sface.Search_interface.initial);
    Limit.unwrap_sol6 unwrap_sol
      (id_fun
	 search_interface
	 better_p
	 see_expansion
	 check_bound
	 iteration_complete)

(************************************************************)
(* Counts number of expansions... doesn't return a result.  *)
(************************************************************)

let no_dups_count sface threshold =
  (** Performs an IDA* search from the initial state to a goal, for
      domains with no duplicates.  Accepts bound estimation
      functions. *)
  let search_interface = Search_interface.make
    ~node_expand:(make_expand
		    sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      h = 0.;
      f = 0.;}
    better_p
    (fun _ -> ()) in
    (Iterative_deepening.no_dups_count
       search_interface
       (fun n -> n.f)
       (fun n -> sface.Search_interface.h n.data)
       threshold)


let no_dups_total_count sface threshold =
  (** Performs an IDA* search from the initial state to a goal.  The
      result is the number of (nodes expanded * nodes expanded in final
      iteration) *)
  no_dups_count sface threshold


(************************************************************)
(* Functions with pre-specified models.                     *)
(************************************************************)


let no_dups sface args =
  (** Performs an IDA* search from the initial state to a goal, for
      domains with no duplicates. *)
  Search_args.is_empty "Iterative_deepening_astar.no_dups" args;
  no_dups_with_model
    Iterative_deepening.no_dups
    sface
    reset_bound_default
    see_expansion_default
    iteration_complete_default
    check_bound_default

let no_dups_bnb sface args =
  (** Performs an IDA* search from the initial state to a goal, for
      domains with no duplicates performing branch-and-bound on the
      final iteration. *)
  Search_args.is_empty "Iterative_deepening_astar.no_dups_bnb" args;
  no_dups_with_model
    Iterative_deepening.no_dups_bnb
    sface
    reset_bound_default
    see_expansion_default
    iteration_complete_default
    check_bound_default

let no_dups_total sface args =
  (** Performs an IDA* search from the initial state to a goal, for
      domains with no duplicates expanding the entire final
      iteration. *)
  Search_args.is_empty "Iterative_deepening_astar.no_dups_total" args;
  no_dups_with_model
    Iterative_deepening.no_dups_total
    sface
    reset_bound_default
    see_expansion_default
    iteration_complete_default
    check_bound_default


let dups_total sface args =
  (** Performs an IDA* search from the initial state to a goal, for
      domains with no duplicates expanding the entire final
      iteration. *)
  Search_args.is_empty "Iterative_deepening_astar.dups_total" args;
  dups_with_model
    Iterative_deepening.no_dups_in_dups_dom_total
    sface
    reset_bound_default
    see_expansion_default
    iteration_complete_default
    check_bound_default


let dups_double_bound sface args =
  (** Performs an IDA* search doubling the bound each iteration. *)
  Search_args.is_empty "Iterative_deepening_astar.dups_double_bound" args;
  dups_with_model
    (Iterative_deepening.no_dups_in_dups_dom)
    sface
    reset_bound_default
    see_expansion_default
    iteration_complete_double
    check_bound_default


let dups_idastar_cr sface args =
  (** Performs an IDA* search doubling the bound each iteration. *)
  let nbins = Search_args.get_int
    "Iterative_deepening_astar.dups_idastar_cr" args 0
  and control_factor = Search_args.get_float
    "Iterative_deepening_astar.dups_idastar_cr" args 1 in
  let get_f n = n.f in
  let reset, see, complete, check =
    Idastar_cr_model.make nbins control_factor get_f
  in
    dups_with_model
      Iterative_deepening.no_dups_in_dups_dom_bnb
      sface reset see complete check


let no_dups_idastar_cr sface args =
  (** Performs an IDA* search doubling the bound each iteration. *)
  let nbins = Search_args.get_int
    "Iterative_deepening_astar.dups_idastar_cr" args 0
  and control_factor = Search_args.get_float
    "Iterative_deepening_astar.dups_idastar_cr" args 1 in
  let get_f n = n.f in
  let reset, see, complete, check =
    Idastar_cr_model.make nbins control_factor get_f
  in
    no_dups_with_model
      Iterative_deepening.no_dups_bnb
      sface reset see complete check


let dups sface args =
  (** Performs an IDA* search from the initial state to a goal, for
      domains with no duplicates. *)
  Search_args.is_empty "Iterative_deepening_astar.dups" args;
  let search_interface = Search_interface.make
    ~node_expand:(make_expand
		    sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      h = 0.;
      f = 0.;}
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    reset_bound_default
      (sface.Search_interface.h sface.Search_interface.initial);
    Limit.unwrap_sol6 unwrap_sol
      (Iterative_deepening.no_dups_in_dups_dom
	 search_interface
	 better_p
	 see_expansion_default
	 check_bound_default
	 iteration_complete_default)


let dups_with_bound sface args =
  (** Performs an IDA* search from the initial state to a goal, for
      domains with no duplicates.  This version is given an initial
      bound as its argument. *)
  let bound = Search_args.get_float "Iterative_deepening_astar.dups" args 0 in
  let iters = ref 0 in
  let search_interface = Search_interface.make
    ~node_expand:(make_expand
		    sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      h = 0.;
      f = 0.;}
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    reset_bound_default
      (sface.Search_interface.h sface.Search_interface.initial);
    Limit.unwrap_sol6 unwrap_sol
      (Iterative_deepening.no_dups_in_dups_dom_total
	 search_interface
	 better_p
	 see_expansion_default
	 (fun n -> n.f <= bound)
	 (fun _ _ ->
	    assert (!iters = 0);
	    incr iters;)
	 (*iteration_complete_default*))

(* EOF *)

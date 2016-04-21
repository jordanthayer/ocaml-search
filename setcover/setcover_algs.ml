(** Algorithms for the setcover problem.

    2010-01-06 -- Copied from tsp_bounded_depth_algs.ml

    @author eaburns
    @since 2010-01-05
*)

open Printf

let get_result ch res =
  (** [get_result ch res] prints and returns the resulting resulting
      info. *)
  let lvl = Verb.toplvl in
  let sol, (nodes, leaves, branches, prunes), optimal, complete = res in
    Verb.pr lvl "nodes:    %d\n" nodes;
    Verb.pr lvl "leaves:   %d\n" leaves;
    Verb.pr lvl "branches: %d\n" branches;
    Verb.pr lvl "prunes:   %d\n" prunes;
    Verb.pr lvl "optimal:  %s\n" (if optimal then "true" else "false");
    Verb.pr lvl "complete: %s\n" (if complete then "true" else "false");
    sol, nodes, leaves, branches, prunes, optimal, complete


let dfs make_interface inst halt_spec logger =
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Dfs.depth_first_search
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let ilds_top make_interface inst halt_spec logger =
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Lds.ilds_top ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.max_depth
	 i.I.is_better i.I.is_leaf
	 i.I.num_children i.I.nth_child init)

let ilds_bottom make_interface inst halt_spec logger =
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Lds.ilds_bottom ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.max_depth i.I.is_better i.I.is_leaf
	 i.I.num_children i.I.nth_child init)


let dds make_interface inst halt_spec logger =
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Dds.search ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let indecision_sum make_interface inst halt_spec logger =
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Indecision.indecision_search
	 i.I.max_depth
	 2
	 i.I.child_costs
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let blfs_sep args make_interface inst halt_spec logger =
  let learner_name = args.(1) in
  let learning_rate = float_of_string args.(2) in
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Blfs_separate.search
	 ~dump_model:false
	 ~learner_name:learner_name
	 ~learning_rate:learning_rate
	 ~halt:halt_spec ~log:logger
	 ~is_optimal:i.I.is_optimal
	 ~should_prune:i.I.should_prune
	 i.I.leaf_cost
	 i.I.max_depth
	 2				(* max children *)
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child
	 init)

let blfs_poly args make_interface inst halt_spec logger =
  let learner_name = args.(1) in
  let learning_rate = float_of_string args.(2) in
  let degree = int_of_string args.(3) in
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Blfs_polynomial.search
	 ~dump_model:false
	 ~learner_name:learner_name
	 ~learning_rate:learning_rate
	 ~is_optimal:i.I.is_optimal
	 ~should_prune:i.I.should_prune
	 ~halt:halt_spec ~log:logger
	 i.I.leaf_cost
	 degree
	 i.I.max_depth
	 2				(* max children *)
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child
	 init)


let no_args f args =
  (** [no_args f args] returns a search function that is built from an
      argument list.  If the argument list is not empty then an error
      is given. *)
  if (Array.length args) > 1
  then
    invalid_arg
      (Wrutils.str "No arguments must be supplied to search algorithm %s"
	 args.(0))
  else f


let by_name name =
  (** [by_name name] gets an algorithm function by its name. *)
  let alg_tab = [
    "dfs", (no_args dfs);
    "ilds_top", (no_args ilds_top);
    "ilds_bottom", (no_args ilds_bottom);
    "dds", (no_args dds);
    "indecision_sum", (no_args indecision_sum);
    "blfs_sep", blfs_sep;
    "blfs_poly", blfs_poly;
  ]
  in
    try List.assoc name alg_tab
    with Not_found ->
      invalid_arg
	(sprintf "Algs are: %s"
	   (List.fold_left
	      (fun s (n, _) -> sprintf "%s, %s" s n)
	      (fst (List.hd alg_tab))
	      (List.tl alg_tab)))


let by_args args = (by_name args.(0)) args
  (** [by_args args] gets an algorithm from an argument array. *)

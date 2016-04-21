(** Algorithms for bounded depth search on the TSP.

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

let dds make_interface inst halt_spec logger =
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Dds.search ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
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


let indecision_sum_bottom args make_interface inst halt_spec logger =
  let bins = int_of_string args.(1) in
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Indecision.indecision_search
	 ~bottom_first:true
	 ~max_bins:bins
	 i.I.max_depth
	 i.I.max_depth			(* max children *)
	 i.I.child_costs
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let indecision_sum_top args make_interface inst halt_spec logger =
  let bins = int_of_string args.(1) in
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Indecision.indecision_search
	 ~bottom_first:false
	 ~max_bins:bins
	 i.I.max_depth
	 i.I.max_depth			(* max children *)
	 i.I.child_costs
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let indecision_rbfs make_interface inst halt_spec logger =
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Indecision_rbfs.rbfs
	 ~is_opt:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.child_costs i.I.nth_child init)


let blfs_sep learner_name learning_rate make_interface inst halt_spec logger =
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Separate.blfs_separate
	 ~learner_name:learner_name
	 ~learning_rate:learning_rate
	 i.I.max_depth
	 i.I.max_depth			(* max children *)
	 i.I.leaf_cost
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let windecision_rbfs args make_interface inst halt_spec logger =
  let coeffs = args.(1) in
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Offline_windecision.rbfs
	 ~halt:halt_spec
	 ~log:logger
	 ~is_opt:i.I.is_optimal
	 ~prune:i.I.should_prune
	 coeffs
	 i.I.max_depth
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.child_costs
	 i.I.nth_child
	 init)


let windecision_sum_bottom args make_interface inst halt_spec logger =
  let coeffs = args.(1) in
  let bins = int_of_string args.(2) in
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Offline_windecision.sum_bottom
	 ~halt:halt_spec
	 ~log:logger
	 ~is_opt:i.I.is_optimal
	 ~prune:i.I.should_prune
	 ~bins
	 coeffs
	 i.I.max_depth
	 i.I.max_depth			(* max children *)
	 i.I.child_costs
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child
	 init)

let windecision_sum_top args make_interface inst halt_spec logger =
  let coeffs = args.(1) in
  let bins = int_of_string args.(2) in
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Offline_windecision.sum_top
	 ~halt:halt_spec
	 ~log:logger
	 ~is_opt:i.I.is_optimal
	 ~prune:i.I.should_prune
	 ~bins
	 coeffs
	 i.I.max_depth
	 i.I.max_depth			(* max children *)
	 i.I.child_costs
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child
	 init)


let train_windecision args make_interface inst halt_spec logger =
  let k = int_of_string args.(1) in
  let nleaves = int_of_string args.(2) in
  let traversal = args.(3) in
  let sample_file = args.(4) in
  let init, i = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result stdout
      (Offline_windecision.get_sample
	 ~k ~nleaves
	 ~halt:halt_spec
	 ~log:logger
	 ~is_opt:i.I.is_optimal
	 ~prune:i.I.should_prune
	 sample_file traversal
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.child_costs
	 i.I.nth_child
	 i.I.leaf_cost
	 i.I.max_depth
	 init)


let blfs_poly args make_interface inst halt_spec logger =
  let learner_name = args.(1) in
  let learning_rate = float_of_string args.(2) in
  let degree = int_of_string args.(3) in
  let dump = (Array.length args) = 5 in
  let module I = Bounded_depth_interface in
  let init, i = make_interface inst in
    get_result stdout
      (Blfs_polynomial.search
	 ~dump_model:dump
	 i.I.leaf_cost
	 degree
	 i.I.max_depth
	 i.I.max_depth			(* max children *)
	 ~learner_name:learner_name
	 ~learning_rate:learning_rate
	 ~is_optimal:i.I.is_optimal
	 ~halt:halt_spec
	 ~log:logger
	 ~should_prune:i.I.should_prune
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child
	 init)

let indecision_cr_bottom args make_interface inst halt_spec logger =
  let max_bins = int_of_string args.(1) in
  let module I = Bounded_depth_interface in
  let init, i = make_interface inst in
    get_result stdout
      (Indecision_cr.indecision_search
	 i.I.max_depth
	 i.I.max_depth			(* max children *)
	 i.I.child_costs
	 ~bottom_first:true
	 ~max_bins
	 ~leaf_cost:i.I.leaf_cost
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child init)

let indecision_cr2_bottom args make_interface inst halt_spec logger =
  let max_bins = int_of_string args.(1) in
  let module I = Bounded_depth_interface in
  let init, i = make_interface inst in
    get_result stdout
      (Indecision_cr2.indecision_search
	 i.I.max_depth
	 i.I.max_depth			(* max children *)
	 i.I.child_costs
	 ~bottom_first:true
	 ~max_bins
	 ~leaf_cost:i.I.leaf_cost
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child init)

let wds_bottom args make_interface inst halt_spec logger =
  let max_bins = int_of_string args.(1) in
  let module I = Bounded_depth_interface in
  let init, i = make_interface inst in
    get_result stdout
      (Wds.search
	 i.I.max_depth
	 i.I.max_depth			(* max children *)
	 i.I.child_costs
	 ~bottom_first:true
	 ~max_bins
	 ~leaf_cost:i.I.leaf_cost
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child init)

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
    "dds", (no_args dds);
    "ilds_top", (no_args ilds_top);
    "ilds_bottom", (no_args ilds_bottom);
    "dds", (no_args dds);
    "indecision_sum_bottom", indecision_sum_bottom;
    "indecision_sum_top", indecision_sum_top;
    "indecision_cr_bottom", indecision_cr_bottom;
    "indecision_cr2_bottom", indecision_cr2_bottom;
    "wds_bottom", wds_bottom;
    "indecision_rbfs", (no_args indecision_rbfs);
    "train_windecision", train_windecision;
    "windecision_rbfs", windecision_rbfs;
    "windecision_sum_bottom", windecision_sum_bottom;
    "windecision_sum_top", windecision_sum_top;
    "blfs_poly", blfs_poly;
    "blfs_sep", (fun args ->
		   if (Array.length args) <> 3
		   then invalid_arg "Need learner name and learning rate";
		   let learner_name = args.(1)
		   and learning_rate = float_of_string args.(2)
		   in blfs_sep learner_name learning_rate);
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

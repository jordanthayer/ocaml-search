(** Run algorithms on two-way number partitioning.

    @author eaburns
    @since 2009-12-29
*)

open Printf


let get_result partitions ch res =
  (** [get_result ch res] prints (if verbose enough) and then returns
      the resulting partitions and information. *)
  let lvl = Verb.toplvl in
  let sol, (nodes, leaves, branches, prunes), optimal, complete = res in
  let print_partitions = Two_partition.make_print_partitions partitions in
  let p = match sol with
    | None -> None
    | Some p ->
	if Verb.level lvl then print_partitions ch p;
	Some (partitions p)
  in
    Verb.pr lvl "nodes:    %d\n" nodes;
    Verb.pr lvl "leaves:   %d\n" leaves;
    Verb.pr lvl "branches: %d\n" branches;
    Verb.pr lvl "prunes:   %d\n" prunes;
    Verb.pr lvl "optimal:  %s\n" (if optimal then "true" else "false");
    Verb.pr lvl "complete: %s\n" (if complete then "true" else "false");
    p, nodes, leaves, branches, prunes, optimal, complete


let dfs make_interface inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Dfs.depth_first_search
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let ilds_top make_interface inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Lds.ilds_top ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.max_depth
	 i.I.is_better i.I.is_leaf
	 i.I.num_children i.I.nth_child init)

let ilds_bottom make_interface inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Lds.ilds_bottom ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.max_depth i.I.is_better i.I.is_leaf
	 i.I.num_children i.I.nth_child init)


let dds make_interface inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Dds.search ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let indecision_sum make_interface inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Indecision.indecision_search
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let blfs_sep
    dump learner_name learning_rate make_interface inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (*
	(Separate.blfs_separate
      *)
      (Blfs_separate.search
	 i.I.leaf_cost
	 i.I.max_depth
	 2				(* max children *)
	 ~dump_model:dump
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

let blfs_sep_sampled
    dump sample_size learner_name learning_rate make_interface
    inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (*
	(Separate.blfs_separate
      *)
      (Blfs_separate_sampled.search
	 i.I.leaf_cost
	 i.I.max_depth
	 2				(* max children *)
	 ~sample_size:sample_size
	 ~dump_model:dump
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


let blfs_poly
    dump degree learner_name learning_rate make_interface
    inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Blfs_polynomial.search
	 ~dump_model:dump
	 i.I.leaf_cost
	 degree
	 i.I.max_depth
	 2				(* max children *)
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

let blfs_poly_cost
    dump degree learner_name learning_rate make_interface
    inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Blfs_poly_cost.search
	 ~dump_model:dump
	 i.I.leaf_cost
	 degree
	 i.I.max_depth
	 2				(* max children *)
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


let blfs_fixed dump path make_interface inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Blfs_fixed_model.search
	 ~dump_model:dump
	 path
	 i.I.max_depth
	 2				(* max children *)
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


let blfs_mixed
    dump degree sample_size learner_name learning_rate make_interface
    inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Blfs_mixed.search
	 ~dump_model:dump
	 i.I.leaf_cost
	 degree
	 i.I.max_depth
	 2				(* max children *)
	 ~sample_size:sample_size
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


let blfs_right
    dump learner_name learning_rate make_interface inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Blfs_right_cost.search
	 i.I.leaf_cost
	 i.I.max_depth
	 2				(* max children *)
	 ~dump_model:dump
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


let blfs_right_quad
    learner_name learning_rate make_interface inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Blfs_right_quad.search
	 i.I.leaf_cost
	 i.I.max_depth
	 2				(* max children *)
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


let blfs_lds make_interface inst halt_spec logger =
  let init, i, partitions = make_interface inst in
  let module I = Bounded_depth_interface in
    get_result partitions stdout
      (Blfs_lds_model.search
	 i.I.leaf_cost
	 i.I.max_depth
	 2				(* max children *)
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
    "blfs_sep", (fun args ->
		   if (Array.length args) > 4
		   then invalid_arg "Need learner name and learning rate";
		   let learner_name = args.(1)
		   and learning_rate = float_of_string args.(2)
		   and dump = (Array.length args) = 4
		   in blfs_sep dump learner_name learning_rate);
    "blfs_sep_sampled", (fun args ->
			   if (Array.length args) > 5
			   then
			     invalid_arg "Need learner name and learning rate";
			   let learner_name = args.(1)
			   and learning_rate = float_of_string args.(2)
			   and sample_size = int_of_string args.(3)
			   and dump = (Array.length args) = 5
			   in blfs_sep_sampled dump sample_size
				learner_name learning_rate);
    "blfs_poly", (fun args ->
		    if (Array.length args) > 5
		    then
		      invalid_arg
			"Need learner name, learning rate and degree";
		    let learner_name = args.(1)
		    and learning_rate = float_of_string args.(2)
		    and degree = int_of_string args.(3)
		    and dump = (Array.length args) = 5
		    in blfs_poly dump degree learner_name learning_rate);
    "blfs_poly_cost", (fun args ->
			 if (Array.length args) > 5
			 then
			   invalid_arg
			     "Need learner name, learning rate and degree";
			 let learner_name = args.(1)
			 and learning_rate = float_of_string args.(2)
			 and degree = int_of_string args.(3)
			 and dump = (Array.length args) = 5
			 in blfs_poly_cost dump degree
			      learner_name learning_rate);
    "blfs_fixed", (fun args ->
		     if (Array.length args) > 3
		     then invalid_arg "Need model path";
		     let path = args.(1)
		     and dump = (Array.length args) = 3
		     in blfs_fixed dump path);
    "blfs_mixed",
    (fun args ->
       if (Array.length args) > 6
       then
	 invalid_arg
	   "Need learner name, learning rate, degree and sample size";
       let learner_name = args.(1)
       and learning_rate = float_of_string args.(2)
       and degree = int_of_string args.(3)
       and sample_size = int_of_string args.(4)
       and dump = (Array.length args) = 6
       in blfs_mixed dump degree sample_size learner_name learning_rate);
    "blfs_right", (fun args ->
		     if (Array.length args) > 4
		     then invalid_arg "Need learner name and learning rate";
		     let learner_name = args.(1)
		     and learning_rate = float_of_string args.(2)
		     and dump = (Array.length args) = 4
		     in blfs_right dump learner_name learning_rate);
    "blfs_right_quad", (fun args ->
			  if (Array.length args) <> 3
			  then invalid_arg
			    "Need learner name and learning rate";
			  let learner_name = args.(1)
			  and learning_rate = float_of_string args.(2)
			  in blfs_right_quad learner_name learning_rate);
    "blfs_lds", (no_args blfs_lds);
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

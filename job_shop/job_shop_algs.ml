(** Algorithms for solving the job shop scheduling problem.

    @author eaburns
    @since 2010-02-18
*)

open Printf

(*
*)
module Job_shop = Jsp

let get_result ch st res =
  (** [get_result ch st res] prints (if verbose enough) and then returns
      the resulting startimes and information. *)
  let lvl = Verb.toplvl in
  let sol, (nodes, leaves, branches, prunes), optimal, complete = res in
    Verb.pr lvl "nodes:    %d\n" nodes;
    Verb.pr lvl "leaves:   %d\n" leaves;
    Verb.pr lvl "branches: %d\n" branches;
    Verb.pr lvl "prunes:   %d\n" prunes;
    Verb.pr lvl "optimal:  %s\n" (if optimal then "true" else "false");
    Verb.pr lvl "complete: %s\n" (if complete then "true" else "false");
    begin match sol with
      | None -> ()
      | Some sol -> ignore (st sol);
    end;
    sol, nodes, leaves, branches, prunes, optimal, complete


let taillard_lb init i st inst halt_spec logger =
  get_result stdout st
    (let lb = Job_shop_instance.taillard_lower_bound inst in
       (Some ({
(*
		Job_shop.stn_copy = Simple_temp_net.empty;
*)
		Job_shop.stn_copy = Job_shop.Stn.create_with 0 [];
		Job_shop.make_span = lb;
	      }),
	(0, 0, 0, 0), false, false))


let dfs init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Dfs.depth_first_search
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let ilds_top init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Lds.ilds_top ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.max_depth
	 i.I.is_better i.I.is_leaf
	 i.I.num_children i.I.nth_child init)

let ilds_bottom init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Lds.ilds_bottom ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.max_depth i.I.is_better i.I.is_leaf
	 i.I.num_children i.I.nth_child init)


let dds init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Dds.search ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let indecision_sum_bottom args init i st inst halt_spec logger =
  let norm = bool_of_string args.(1) in
  let max_bins = int_of_string args.(2) in
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Indecision.indecision_search
	 ~norm
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 ~bottom_first:true
	 ~max_bins
	 ~leaf_cost:i.I.leaf_cost
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let indecision_sum_top args init i st inst halt_spec logger =
  let norm = bool_of_string args.(1) in
  let max_bins = int_of_string args.(2) in
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Indecision.indecision_search
	 ~norm
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 ~bottom_first:false
	 ~max_bins
	 ~leaf_cost:i.I.leaf_cost
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let wds_bottom args init i st inst halt_spec logger =
  let max_bins = int_of_string args.(1) in
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Wds.search
	 ~max_bins
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 ~bottom_first:true
	 ~leaf_cost:i.I.leaf_cost
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let wds_top args init i st inst halt_spec logger =
  let max_bins = int_of_string args.(1) in
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Wds.search
	 ~bottom_first:false
	 ~leaf_cost:i.I.leaf_cost
	 ~optimal_p:i.I.is_optimal
	 ~halt:halt_spec
	 ~log:logger
	 ~prune_p:i.I.should_prune
	 ~max_bins
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child
	 init)

let indecision_cr_bottom args init i st inst halt_spec logger =
  let max_bins = int_of_string args.(1) in
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Indecision_cr.indecision_search
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 ~bottom_first:true
	 ~max_bins
	 ~leaf_cost:i.I.leaf_cost
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let indecision_cr2_bottom args init i st inst halt_spec logger =
  let max_bins = int_of_string args.(1) in
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Indecision_cr2.indecision_search
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 ~bottom_first:true
	 ~max_bins
	 ~leaf_cost:i.I.leaf_cost
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let indecision_cr_top args init i st inst halt_spec logger =
  let max_bins = int_of_string args.(1) in
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Indecision_cr.indecision_search
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 ~bottom_first:false
	 ~max_bins
	 ~leaf_cost:i.I.leaf_cost
	 ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
	 ~prune_p:i.I.should_prune
	 i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
	 i.I.nth_child init)


let indecision_max args init i st inst halt_spec logger =
  let max_bins = int_of_string args.(1) in
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Indecision_max.max_indecision_search
	 ~max_bins
	 ~optimal_p:i.I.is_optimal
	 ~prune_p:i.I.should_prune
	 ~halt:halt_spec
	 ~log:logger
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child
	 init)


let indecision_rbfs args init i st inst halt_spec logger =
  let norm = bool_of_string args.(1) in
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Indecision_rbfs.rbfs
	 ~norm
	 ~halt:halt_spec
	 ~log:logger
	 ~is_opt:i.I.is_optimal
	 ~prune:i.I.should_prune
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.child_costs
	 i.I.nth_child
	 init)


let indecision_enum init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Indecision_enum.search
	 ~halt:halt_spec
	 ~log:logger
	 ~is_opt:i.I.is_optimal
	 ~prune:i.I.should_prune
	 i.I.copy_state
	 i.I.leaf_cost
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.child_costs
	 i.I.nth_child
	 init)


let blfs_wted_indecision
    dump learner_name learning_rate init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
      (*
	(Separate.blfs_separate
      *)
      (Blfs_wted_indecision.search
	 i.I.leaf_cost
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 ~max_bins:2000
	 ~dump_model:dump
	 ~learner_name:learner_name
	 ~learning_rate:learning_rate
	 ~is_optimal:i.I.is_optimal
	 ~halt:halt_spec
	 ~log:logger
	 (*
	   ~should_prune:i.I.should_prune
	 *)
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child
	 init)


let blfs_quad_indecision
    dump learner_name learning_rate init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
      (*
	(Separate.blfs_separate
      *)
      (Blfs_quad_indecision.search
	 i.I.leaf_cost
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 ~max_bins:200
	 ~num_probes:10
	 ~dump_model:dump
	 ~learner_name:learner_name
	 ~learning_rate:learning_rate
	 ~is_optimal:i.I.is_optimal
	 ~halt:halt_spec
	 ~log:logger
	 (*
	   ~should_prune:i.I.should_prune
	 *)
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child
	 init)


let blfs_quad_right_indecision
    dump learner_name learning_rate init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
      (*
	(Separate.blfs_separate
      *)
      (Blfs_quad_right_indecision.search
	 i.I.leaf_cost
	 i.I.max_depth
	 2				(* max children *)
	 i.I.child_costs
	 ~max_bins:20000
	 ~dump_model:dump
	 ~learner_name:learner_name
	 ~learning_rate:learning_rate
	 ~is_optimal:i.I.is_optimal
	 ~halt:halt_spec
	 ~log:logger
	 (*
	   ~should_prune:i.I.should_prune
	 *)
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child
	 init)


let blfs_sep
    dump learner_name learning_rate init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
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
    dump sample_size learner_name learning_rate init i st
    inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
      (*
	(Separate.blfs_separate
      *)
      (Blfs_separate_sampled.search
	 i.I.leaf_cost
	 i.I.max_depth
	 2				(* max children *)
	 ~num_probes:10
	 ~max_bins:2000
	 ~initialize_model:false
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
    dump degree learner_name learning_rate init i st
    inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
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
    dump degree learner_name learning_rate init i st
    inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
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


let blfs_fixed dump path init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
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
    dump degree sample_size learner_name learning_rate init i st
    inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
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
    dump learner_name learning_rate init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
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
    learner_name learning_rate init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
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


let blfs_lds init i st inst halt_spec logger =
  let module I = Bounded_depth_interface in
    get_result stdout st
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


let train_windecision args init i st inst halt_spec logger =
  let k = int_of_string args.(1) in
  let nleaves = int_of_string args.(2) in
  let traversal = args.(3) in
  let sample_file = args.(4) in
  let module I = Bounded_depth_interface in
    get_result stdout st
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


let windecision_rbfs args init i st inst halt_spec logger =
  let coeffs = args.(1) in
  let module I = Bounded_depth_interface in
    get_result stdout st
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


let windecision_sum_bottom args init i st inst halt_spec logger =
  let coeffs = args.(1) in
  let bins = int_of_string args.(2) in
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Offline_windecision.sum_bottom
	 ~halt:halt_spec
	 ~log:logger
	 ~is_opt:i.I.is_optimal
	 ~prune:i.I.should_prune
	 ~bins
	 coeffs
	 i.I.max_depth
	 2				(* children *)
	 i.I.child_costs
	 i.I.copy_state
	 i.I.is_better
	 i.I.is_leaf
	 i.I.num_children
	 i.I.nth_child
	 init)


let windecision_sum_top args init i st inst halt_spec logger =
  let coeffs = args.(1) in
  let bins = int_of_string args.(2) in
  let module I = Bounded_depth_interface in
    get_result stdout st
      (Offline_windecision.sum_top
	 ~halt:halt_spec
	 ~log:logger
	 ~is_opt:i.I.is_optimal
	 ~prune:i.I.should_prune
	 ~bins
	 coeffs
	 i.I.max_depth
	 2				(* children *)
	 i.I.child_costs
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
    "taillard_lb", (no_args taillard_lb);
    "dfs", (no_args dfs);
    "ilds_top", (no_args ilds_top);
    "ilds_bottom", (no_args ilds_bottom);
    "dds", (no_args dds);
    "indecision_enum", (no_args indecision_enum);
    "indecision_rbfs", indecision_rbfs;
    "indecision_sum_bottom", indecision_sum_bottom;
    "indecision_sum_top", indecision_sum_top;
    "wds_bottom", wds_bottom;
    "wds_top", wds_top;
    "indecision_cr_bottom", indecision_cr_bottom;
    "indecision_cr2_bottom", indecision_cr2_bottom;
    "indecision_cr_top", indecision_cr_top;
    "indecision_max", indecision_max;
    "train_windecision", train_windecision;
    "windecision_rbfs", windecision_rbfs;
    "windecision_sum_bottom", windecision_sum_bottom;
    "windecision_sum_top", windecision_sum_top;
    "blfs_wted_indecision",
    (fun args ->
       if (Array.length args) > 4
       then invalid_arg "Need learner name and learning rate";
       let learner_name = args.(1)
       and learning_rate = float_of_string args.(2)
       and dump = (Array.length args) = 4
       in blfs_wted_indecision dump learner_name learning_rate);
    "blfs_quad_indecision",
    (fun args ->
       if (Array.length args) > 4
       then invalid_arg "Need learner name and learning rate";
       let learner_name = args.(1)
       and learning_rate = float_of_string args.(2)
       and dump = (Array.length args) = 4
       in blfs_quad_indecision dump learner_name learning_rate);
    "blfs_quad_right_indecision",
    (fun args ->
       if (Array.length args) > 4
       then invalid_arg "Need learner name and learning rate";
       let learner_name = args.(1)
       and learning_rate = float_of_string args.(2)
       and dump = (Array.length args) = 4
       in blfs_quad_right_indecision dump learner_name learning_rate);
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

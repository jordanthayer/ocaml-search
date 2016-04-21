(** The algorithms for running the bounded-depth knapsack solver.

    I hate this module name too.  Don't judge me.

    @author eaburns
    @since 2011-01-10
*)

open Printf
open Knapsack_bounded_depth

let dfs inst halt log =
  let init = make_init inst in
    Dfs.depth_first_search
      ~halt ~log
      ~optimal_p:is_optimal
      ~prune_p:should_prune
      copy_state
      is_better
      (make_is_leaf inst)
      (make_num_children inst)
      (make_nth_child inst)
      init


let ilds_top inst halt log =
  let init = make_init inst in
    Lds.ilds_top
      ~halt ~log
      ~optimal_p:is_optimal
      ~prune_p:should_prune
      copy_state
      (max_depth inst)
      is_better
      (make_is_leaf inst)
      (make_num_children inst)
      (make_nth_child inst)
      init


let ilds_bottom inst halt log =
  let init = make_init inst in
    Lds.ilds_bottom
      ~halt ~log
      ~optimal_p:is_optimal
      ~prune_p:should_prune
      copy_state
      (max_depth inst)
      is_better
      (make_is_leaf inst)
      (make_num_children inst)
      (make_nth_child inst)
      init


let indecision_sum_bottom args inst halt log =
  let max_bins = int_of_string args.(1) in
  let init = make_init inst in
    Indecision.indecision_search
      ~bottom_first:true
      ~max_bins
      ~halt ~log
      ~optimal_p:is_optimal
      ~prune_p:should_prune
      (max_depth inst)
      (max_children inst)
      (make_child_costs inst)
      copy_state
      is_better
      (make_is_leaf inst)
      (make_num_children inst)
      (make_nth_child inst)
      init


let indecision_sum_top args inst halt log =
  let max_bins = int_of_string args.(1) in
  let init = make_init inst in
    Indecision.indecision_search
      ~bottom_first:false
      ~max_bins
      ~halt ~log
      ~optimal_p:is_optimal
      ~prune_p:should_prune
      (max_depth inst)
      (max_children inst)
      (make_child_costs inst)
      copy_state
      is_better
      (make_is_leaf inst)
      (make_num_children inst)
      (make_nth_child inst)
      init


let indecision_rbfs inst halt log =
  let init = make_init inst in
    Indecision_rbfs.rbfs
      ~halt ~log
      ~is_opt:is_optimal
      ~prune:should_prune
      copy_state
      is_better
      (make_is_leaf inst)
      (make_num_children inst)
      (make_child_costs inst)
      (make_nth_child inst)
      init


let windecision_rbfs args inst halt log =
  let coeffs = args.(1) in
  let init = make_init inst in
    Offline_windecision.rbfs
      ~halt ~log
      ~is_opt:is_optimal
      ~prune:should_prune
      coeffs
      (max_depth inst)
      copy_state
      is_better
      (make_is_leaf inst)
      (make_num_children inst)
      (make_child_costs inst)
      (make_nth_child inst)
      init


let windecision_sum_bottom args inst halt log =
  let coeffs = args.(1) in
  let bins = int_of_string args.(2) in
  let init = make_init inst in
    Offline_windecision.sum_bottom
      ~halt ~log
      ~is_opt:is_optimal
      ~prune:should_prune
      ~bins
      coeffs
      (max_depth inst)
      (max_children inst)
      (make_child_costs inst)
      copy_state
      is_better
      (make_is_leaf inst)
      (make_num_children inst)
      (make_nth_child inst)
      init


let windecision_sum_top args inst halt log =
  let coeffs = args.(1) in
  let bins = int_of_string args.(2) in
  let init = make_init inst in
    Offline_windecision.sum_top
      ~halt ~log
      ~is_opt:is_optimal
      ~prune:should_prune
      ~bins
      coeffs
      (max_depth inst)
      (max_children inst)
      (make_child_costs inst)
      copy_state
      is_better
      (make_is_leaf inst)
      (make_num_children inst)
      (make_nth_child inst)
      init


let train_windecision args inst halt log =
  let k = int_of_string args.(1) in
  let nleaves = int_of_string args.(2) in
  let traversal = args.(3) in
  let sample_file = args.(4) in
  let init = make_init inst in
    Offline_windecision.get_sample
      ~k ~nleaves
      ~halt ~log
      ~is_opt:is_optimal
      ~prune:should_prune
      sample_file
      traversal
      copy_state
      is_better
      (make_is_leaf inst)
      (make_num_children inst)
      (make_child_costs inst)
      (make_nth_child inst)
      leaf_cost
      (max_depth inst)
      init


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
    (*
      "dds", (no_args dds);
    *)
    "indecision_sum_bottom", indecision_sum_bottom;
    "indecision_sum_top", indecision_sum_top;
    "indecision_rbfs", (no_args indecision_rbfs);
    "train_windecision", train_windecision;
    "windecision_rbfs", windecision_rbfs;
    "windecision_sum_bottom", windecision_sum_bottom;
    "windecision_sum_top", windecision_sum_top;
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

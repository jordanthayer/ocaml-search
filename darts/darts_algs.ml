(** Algorithms that can be used to solve the darts problem.

    @author eaburns
    @since 2010-04-08
*)

open Printf

let max_kids = 5000


let dfs inst halt_spec logger =
  let init = Darts.make_initial inst in
  let i = Darts.make_interface inst in
  let module I = Bounded_depth_interface in
    Dfs.depth_first_search
      ~optimal_p:i.I.is_optimal ~halt:halt_spec~log:logger
      ~prune_p:i.I.should_prune
      i.I.copy_state
      i.I.is_better
      i.I.is_leaf
      i.I.num_children
      i.I.nth_child
      init

let ilds_bottom inst halt_spec logger =
  let init = Darts.make_initial inst in
  let i = Darts.make_interface inst in
  let module I = Bounded_depth_interface in
    Lds.ilds_bottom
      ~optimal_p:i.I.is_optimal ~halt:halt_spec~log:logger
      ~prune_p:i.I.should_prune
      i.I.copy_state i.I.max_depth
      i.I.is_better
      i.I.is_leaf
      i.I.num_children
      i.I.nth_child
      init


let ilds_top inst halt_spec logger =
  let init = Darts.make_initial inst in
  let i = Darts.make_interface inst in
  let module I = Bounded_depth_interface in
    Lds.ilds_top
      ~optimal_p:i.I.is_optimal ~halt:halt_spec~log:logger
      ~prune_p:i.I.should_prune
      i.I.copy_state i.I.max_depth
      i.I.is_better
      i.I.is_leaf
      i.I.num_children
      i.I.nth_child
      init


let indecision_rbfs args inst halt_spec logger =
  let norm = bool_of_string args.(1) in
  let init = Darts.make_initial inst in
  let i = Darts.make_interface inst in
  let module I = Bounded_depth_interface in
    Indecision_rbfs.rbfs
      ~norm ~is_opt:i.I.is_optimal ~halt:halt_spec ~log:logger
      ~prune:i.I.should_prune
      i.I.copy_state
      i.I.is_better
      i.I.is_leaf
      i.I.num_children
      i.I.child_costs
      i.I.nth_child
      init


let windecision_rbfs args inst halt_spec logger =
  let coeffs = args.(1) in
  let init = Darts.make_initial inst in
  let i = Darts.make_interface inst in
  let module I = Bounded_depth_interface in
    Offline_windecision.rbfs
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
      init


let windecision_sum_bottom args inst halt_spec logger =
  let coeffs = args.(1) in
  let bins = int_of_string args.(2) in
  let init = Darts.make_initial inst in
  let i = Darts.make_interface inst in
  let module I = Bounded_depth_interface in
    Offline_windecision.sum_bottom
      ~halt:halt_spec
      ~log:logger
      ~is_opt:i.I.is_optimal
      ~prune:i.I.should_prune
      ~bins
      coeffs
      i.I.max_depth
      max_kids
      i.I.child_costs
      i.I.copy_state
      i.I.is_better
      i.I.is_leaf
      i.I.num_children
      i.I.nth_child
      init

let windecision_sum_top args inst halt_spec logger =
  let coeffs = args.(1) in
  let bins = int_of_string args.(2) in
  let init = Darts.make_initial inst in
  let i = Darts.make_interface inst in
  let module I = Bounded_depth_interface in
    Offline_windecision.sum_top
      ~halt:halt_spec
      ~log:logger
      ~is_opt:i.I.is_optimal
      ~prune:i.I.should_prune
      ~bins
      coeffs
      i.I.max_depth
      max_kids
      i.I.child_costs
      i.I.copy_state
      i.I.is_better
      i.I.is_leaf
      i.I.num_children
      i.I.nth_child
      init


let train_windecision args inst halt_spec logger =
  let k = int_of_string args.(1) in
  let nleaves = int_of_string args.(2) in
  let traversal = args.(3) in
  let sample_file = args.(4) in
  let init = Darts.make_initial inst in
  let i = Darts.make_interface inst in
  let module I = Bounded_depth_interface in
    Offline_windecision.get_sample
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
      init


let indecision_enum inst halt_spec logger =
  let init = Darts.make_initial inst in
  let i = Darts.make_interface inst in
  let module I = Bounded_depth_interface in
    Indecision_enum.search
      ~is_opt:i.I.is_optimal ~halt:halt_spec ~log:logger
      ~prune:i.I.should_prune
      i.I.copy_state
      i.I.leaf_cost
      i.I.is_better
      i.I.is_leaf
      i.I.num_children
      i.I.child_costs
      i.I.nth_child
      init


let indecision_sum_bottom args inst halt_spec logger =
  let norm = bool_of_string args.(1) in
  let max_bins = int_of_string args.(2) in
  let init = Darts.make_initial inst in
  let i = Darts.make_interface inst in
  let module I = Bounded_depth_interface in
    Indecision.indecision_search
      ~norm
      i.I.max_depth
      (* i.I.max_children *) max_kids
      i.I.child_costs
      ~bottom_first:true
      ~max_bins
      ~leaf_cost:i.I.leaf_cost
      ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
      ~prune_p:i.I.should_prune
      i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
      i.I.nth_child init


let indecision_sum_top args inst halt_spec logger =
  let norm = bool_of_string args.(1) in
  let max_bins = int_of_string args.(2) in
  let init = Darts.make_initial inst in
  let i = Darts.make_interface inst in
  let module I = Bounded_depth_interface in
    Indecision.indecision_search
      ~norm
      i.I.max_depth
      (* i.I.max_children *) max_kids
      i.I.child_costs
      ~bottom_first:false
      ~max_bins
      ~leaf_cost:i.I.leaf_cost
      ~optimal_p:i.I.is_optimal ~halt:halt_spec ~log:logger
      ~prune_p:i.I.should_prune
      i.I.copy_state i.I.is_better i.I.is_leaf i.I.num_children
      i.I.nth_child init


let no_args f args =
  (** [no_args f args] returns a search function that is built from an
      argument list.  If the argument list is not empty then an error
      is given. *)
  if (Array.length args) > 1 then
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
    "indecision_enum", (no_args indecision_enum);
    "indecision_rbfs", indecision_rbfs;
    "indecision_sum_bottom", indecision_sum_bottom;
    "indecision_sum_top", indecision_sum_top;
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

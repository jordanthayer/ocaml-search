(** A BLFS separate cost model.

    @author eaburns
    @since 2010-01-12
*)

type t = {
  costs : float array;
  (* An array of the costs coefficients from the learner. *)

  pattern : float array;
  (* Pattern of non-zero rank children taken during the search. *)

  mutable root_cost : float;
  (* The initial cost value of the root node. *)

  tree : Blfs_tree_stats.t;
  (* Various statistics on the tree structure: terms, non-terms,
     branching. *)

  accumulate_deviation : float -> unit;
  output_average_deviation : unit -> unit;
  (* Tracking average deviation in the learning. *)

  max_bins : int;
  (* Maximum number of histogram bins to use. *)

  show_leaf_cost : float -> float;
  (* Shows a leaf cost to the learner results in the error. *)

  get_coeffs : unit -> float array;
  (* Gets the coefficients from the learner. *)
}

let index_max_ch ~max_children ~rank ~depth =
    (depth * max_children) + rank


let index model ~rank ~depth =
  let max_children = model.tree.Blfs_tree_stats.max_children in
    index_max_ch ~max_children ~rank ~depth


let make learner ~max_bins ~max_children ~max_depth =
  (** [make learner ~max_bins ~max_children ~max_depth] makes a new
      model. *)
  let n = max_children * max_depth in
  let pattern = Array.make n 0. in
  let show_learner, _, get_coeffs = learner n in
  let show_leaf_cost cost =
    let _, err = show_learner pattern cost in
      err
  in
  let accum_dev, output_dev = Blfs_regression.make_error_tracker "" in
    {
      costs = Array.make n 0.;
      pattern = pattern;
      root_cost = 0.;
      tree = Blfs_tree_stats.make ~max_children ~max_depth;
      accumulate_deviation = accum_dev;
      output_average_deviation = (fun () -> ignore (output_dev ()));
      max_bins = max_bins;
      show_leaf_cost = show_leaf_cost;
      get_coeffs = get_coeffs;
    }


let clamp_coeffs model =
  (** [clamp_coeffs model] clamps the learner's weights to be
      monotonically increasing, etc. *)
  let max_depth = model.tree.Blfs_tree_stats.max_depth
  and max_children = model.tree.Blfs_tree_stats.max_children in
  let coeffs = model.get_coeffs () in
    Blfs_regression.clamp_monotonic coeffs max_depth max_children;
    let rc =
      Blfs_regression.normalize_for_zero_completions
	coeffs max_depth max_children
    in model.root_cost <- rc


let plot_model model () =
  (** [plot_model model ()] makes a plot of the separate model. *)
  let costs = model.get_coeffs () in
  let max_depth = model.tree.Blfs_tree_stats.max_depth
  and max_children = model.tree.Blfs_tree_stats.max_children in
    clamp_coeffs model;
    Blfs_plot_model.plot_separate
      ~init_depth:1 max_depth max_children costs "separate"


let rec n_free max_depth max_children costs depth =
  if depth = max_depth
  then 1
  else begin
    let n_zero_branches = ref 0 in
      for r = 0 to max_children - 1 do
	if costs.(index_max_ch ~max_children ~rank:r ~depth) <= 0.
	then incr n_zero_branches
      done;
      let below =  n_free max_depth max_children costs (depth + 1) in
	1 + below * !n_zero_branches
  end


let make_see_branch info model =
  (** [make_see_branch info model] makes a function that updates the
      model on a branching node at the given depth. *)
  (fun depth node ->
     let nchildren = Info.num_children info node in
       Blfs_tree_stats.see_branch model.tree ~depth ~nchildren)


let make_see_terminal model =
  (** [make_see_terminal model] makes a function that updates the
      model on a leaf node or a prune at the given depth. *)
  (fun _ (_, depth) -> Blfs_tree_stats.see_terminal model.tree depth)


let make_see_leaf leaf_cost model =
  (** [make_see_leaf leaf_cost model] makes a function that updates
      the model on a leaf node at the given depth. *)
  (fun node (cost, depth) ->
     Blfs_tree_stats.see_terminal model.tree depth;
     let err = abs_float (model.show_leaf_cost (leaf_cost node)) in
       model.accumulate_deviation err)


let edge_cost model ~rank ~depth =
  (** [edge_cost model rank depth] computes the cost of an
      edge. *)
  let init_c = if depth = 0 then model.root_cost else 0.
  in  init_c +. model.costs.(index model ~rank ~depth)


let make_remember_edge model =
  (** [make_remember_edge model] makes a function that remembers the
      fact that the search has taken the edge (depth, rank). *)
  (fun depth rank _ -> model.pattern.(index model ~rank ~depth) <- 1.0)


let make_forget_edge model =
  (** [make_forget_edge model] makes a function that forgets the fact
      that the search has taken the edge (depth, rank). *)
  (fun depth rank _ -> model.pattern.(index model ~rank ~depth) <- 0.0)


let prepare_for_search model =
  (** [prepare_for_search model] prepares the model for another
      iteration of search. *)
  (* prepare model.costs before finding the bound (which happens
     before this). *)
  model.output_average_deviation ();
  Wrarray.fill_all model.pattern 0.;
  Blfs_tree_stats.clear model.tree


let make_initial_data model =
  (** [make_initial_data model] makes a function that gets the root
      node for the beginning of an iteration of BLFS search. *)
  (fun _ ->
     prepare_for_search model;
     0., 0)


let make_find_bound model =
  (** [make_find_bound model] makes a function that finds the bound
      that should give the desired number of node expansions. *)
  let max_children = model.tree.Blfs_tree_stats.max_children in
  let get_child_costs parent_costs depth bound =
    let min_parent_g = Hist.min_val parent_costs in
    let c_hist = Hist.make model.max_bins in
    let base = depth * max_children in
    let costs = model.costs in
      for r = 0 to max_children - 1 do
	let c = costs.(base + r) in
	let p = Blfs_tree_stats.p_with_child_rank model.tree ~depth ~rank:r in
	  if min_parent_g +. c <= bound then Hist.add_mass c p c_hist;
      done;
      c_hist
  in
    (fun desired ->
       let coeffs = model.get_coeffs () in
	 clamp_coeffs model;
	 Wrarray.copy_into coeffs model.costs;
(*
	 let n_zero =
	   n_free
	     model.tree.Blfs_tree_stats.max_depth max_children model.costs 1
	 in
	   Wrutils.pr "%d zero cost nodes\n" n_zero;
*)
	   Blfs_bound.find_bound
	     ~max_bins:model.max_bins
	     (Blfs_tree_stats.branch_prob model.tree)
	     get_child_costs
	     ~root_cost:model.root_cost
	     ~desired)


let search
    leaf_cost
    max_depth
    max_children
    ?(dump_model=false)
    ?(learner_name = "lms")
    ?(learning_rate = 0.001)
    ?(max_bins = 200)
    ?(num_probes = 10)
    ?(is_optimal = (Fn.constantly1 false))
    ?(should_prune = (Fn.constantly2 false))
    ?(prev_best = None)
    ?(halt = [Info.Never])
    ?(log = Fn.no_op2)
    copy_state
    is_better
    is_leaf
    num_children
    nth_child
    initial =
  (** [search leaf_cost max_depth max_children ?learner_name
      ?learning_rate ?max_bins ?num_probes ?is_optimal ?should_prune
      ?prev_best ?halt ?log copy_state is_better is_leaf num_children
      nth_child initial] performs a BLFS search using the quadratic
      regression model. *)
  let learner =
    Blfs_regression.learner_by_name learner_name learning_rate
  and info =
    Info.make num_children nth_child is_leaf is_better is_optimal
      copy_state should_prune log prev_best halt
  in
  let model = make learner max_bins max_children max_depth in
  let initial_data = make_initial_data model
  and see_branch = make_see_branch info model
  and see_terminal = make_see_terminal model
  and see_leaf = make_see_leaf leaf_cost model
  and remember_edge = make_remember_edge model
  and forget_edge = make_forget_edge model
  and find_bound = make_find_bound model in
  let iter_children =
    Blfs_regression.make_iter_children info see_branch
      (fun depth rank _ -> edge_cost model ~rank ~depth) (* edge cost *)
      (fun depth rank -> 0.) (* best completion *)
      remember_edge
      forget_edge
  in
  let blfs_interface =
    Blfs.make_interface
      ~dump:(if dump_model then (plot_model model) else (fun () -> ()))
      initial_data see_leaf see_terminal iter_children find_bound
  in

(*
  let f, _ =
    Blfs_regression.make_initializer info 2 max_depth max_children
      leaf_cost see_terminal see_branch initial
  in
  let coeffs = model.get_coeffs () in
    for r = 0 to max_children - 1 do
      for d = 0 to max_depth - 1 do
	coeffs.(index model ~rank:r ~depth:d) <- Math.fmax (f d r) 0.
      done;
    done;
*)
    prepare_for_search model;
    Blfs_regression.random_probes info max_children see_branch
      see_leaf see_terminal remember_edge forget_edge initial num_probes;
    Blfs.best_leaf_first_search blfs_interface info 0. initial

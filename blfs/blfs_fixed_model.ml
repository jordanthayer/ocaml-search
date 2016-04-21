(** A BLFS separate cost model.

    @author eaburns
    @since 2010-01-12
*)

type t = {
  costs : float array;
  (* An array of the costs coefficients from the learner. *)

  root_cost : float;
  (* The cost at the root node. *)

  tree : Blfs_tree_stats.t;
  (* Various statistics on the tree structure: terms, non-terms,
     branching. *)
  max_bins : int;
  (* Maximum number of histogram bins to use. *)
}

let index model ~rank ~depth =
  let max_children = model.tree.Blfs_tree_stats.max_children in
    (depth * max_children) + rank


let make costs_path ~max_bins ~max_children ~max_depth =
  (** [make costs_path ~max_bins ~max_children ~max_depth] makes a new
      model. *)
  let inch = open_in costs_path in
  let costs = (Marshal.from_channel inch : float array) in
    close_in inch;
    Blfs_regression.clamp_monotonic costs max_depth max_children;
    let rc =
      Blfs_regression.normalize_for_zero_completions
	costs max_depth max_children;
    in
      {
	costs = costs;
	root_cost = rc;
	tree = Blfs_tree_stats.make ~max_children ~max_depth;
	max_bins = max_bins;
      }


let plot_model model () =
  (** [plot_model model ()] makes a plot of the separate model. *)
  let costs = model.costs in
  let max_depth = model.tree.Blfs_tree_stats.max_depth
  and max_children = model.tree.Blfs_tree_stats.max_children in
    Blfs_plot_model.plot_separate
      ~init_depth:1 max_depth max_children costs "separate"


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


let make_see_leaf = make_see_terminal
  (** [make_see_leaf model] makes a function that updates the model on
      a leaf node at the given depth. *)


let edge_cost model ~rank ~depth =
  (** [edge_cost model rank depth] computes the cost of an
      edge. *)
  let init_c = if depth = 0 then model.root_cost else 0.
  in init_c +. model.costs.(index model ~rank ~depth)


let make_remember_edge model depth rank cost = ()
  (** [make_remember_edge model depth rank] makes a function that
      remembers the fact that the search has taken the edge (depth,
      rank). *)


let make_forget_edge model depth rank cost = ()
  (** [make_forget_edge model depth rank] makes a function that
      forgets the fact that the search has taken the edge (depth,
      rank). *)


let prepare_for_search model =
  (** [prepare_for_search model] prepares the model for another
      iteration of search. *)
  Blfs_tree_stats.clear model.tree


let make_initial_data model =
  (** [make_initial_data model] makes a function that gets the root
      node for the beginning of an iteration of BLFS search. *)
  (fun _ ->
     prepare_for_search model;
     0., 0)


let make_find_bound model desired =
  (** [make_find_bound model desired] makes a function that finds the
      bound that should give the desired number of node expansions. *)
  let max_children = model.tree.Blfs_tree_stats.max_children in
  let get_child_costs parent_costs depth bound =
    let min_parent_g = Hist.min_val parent_costs in
    let c_hist = Hist.make model.max_bins in
    let base = depth * max_children in
    let costs = model.costs in
      for r = 0 to max_children - 1 do
	let c = costs.(base + r) in
	let p = Blfs_tree_stats.p_with_child_rank model.tree ~depth ~rank:r in
	  if min_parent_g +. c <= bound
	  then Hist.add_mass c p c_hist;
      done;
      c_hist
  in
    Blfs_bound.find_bound
      ~max_bins:model.max_bins
      (Blfs_tree_stats.branch_prob model.tree)
      get_child_costs
      ~root_cost:model.root_cost
      ~desired


let search
    costs_path
    max_depth
    max_children
    ?(dump_model=false)
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
  let info =
    Info.make num_children nth_child is_leaf is_better is_optimal
      copy_state should_prune log prev_best halt
  in
  let model = make costs_path max_bins max_children max_depth in
  let initial_data = make_initial_data model
  and see_branch = make_see_branch info model
  and see_terminal = make_see_terminal model
  and see_leaf = make_see_leaf model
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
    Blfs_regression.random_probes info max_children see_branch
      see_leaf see_terminal remember_edge forget_edge initial num_probes;
    Blfs.best_leaf_first_search blfs_interface info 0. initial

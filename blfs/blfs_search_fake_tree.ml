(**

    @author eaburns
    @since 2010-01-11
*)

type t = {
  max_bins : int;
  (* Maximum number of histogram bins to use. *)

  max_depth : int;

  max_children : int;

  cost_fun : int -> int -> float
}


let make cost_fun max_bins max_children max_depth =
  (** [make max_bins max_children max_depth] makes a new quadratic
      model. *)
  {
    max_bins = 200;
    max_children = 2;
    max_depth = max_depth;
    cost_fun = cost_fun;
  }


let make_see_branch info model =
  (** [make_see_branch info model] makes a function that updates the
      model on a branching node at the given depth. *)
  (fun depth node -> ())


let make_see_terminal model =
  (** [make_see_terminal model] makes a function that updates the
      model on a leaf node or a prune at the given depth. *)
  (fun _ (_, depth) -> ())


let make_see_leaf leaf_cost model =
  (** [make_see_leaf leaf_cost model] makes a function that updates
      the model on a leaf node at the given depth. *)
  (fun node (_, depth) ->
     Wrutils.pr "leaf, cost=%f\n" (leaf_cost node)
  )


let make_remember_edge model =
  (** [make_remember_edge model] makes a function that remembers the
      fact that the search has taken the edge (depth, rank). *)
  (fun depth rank _ -> ())


let make_forget_edge model =
  (** [make_forget_edge model] makes a function that forgets the fact
      that the search has taken the edge (depth, rank). *)
  (fun depth rank _ -> ())


let edge_cost model = model.cost_fun


let prepare_for_search model = ()


let make_initial_data model =
  (** [make_initial_data model] makes a function that gets the root
      node for the beginning of an iteration of BLFS search. *)
  (fun _ ->
     prepare_for_search model;
     0., 0)


let branch_prob model depth = if depth < model.max_depth then 1.0 else 0.


let make_find_bound0 model =
  (** [make_find_bound model] makes a function that finds the bound
      that should give the desired number of node expansions. *)
  let max_bins = model.max_bins in
  let get_child_cost depth rank max_cost =
    let c = model.cost_fun depth rank in
      if c <= max_cost
      then Some (Blfs_bound_old.init_hist max_bins c)
      else None
  in
  let get_child_h depth rank = None
  in
  let nodes_at_depth d = truncate (2. ** (float model.max_depth)) in
  let terms =
    Array.init (model.max_depth + 1) (fun d ->
					if d = model.max_depth
					then nodes_at_depth d
					else 0)
  and non_terms =
    Array.init (model.max_depth + 1) (fun d ->
					if d <> model.max_depth
					then nodes_at_depth d
					else 0)
  in
    (fun desired ->
       Blfs_bound_old.find_bound_for_nodes
	 model.max_depth
	 model.max_children
	 model.max_bins
	 0.				(* root_cost *)
	 0.				(* root_f *)
	 ~get_child_cost
	 ~get_child_h
	 non_terms
	 terms
	 desired)


let make_find_bound model =
  (** [make_find_bound2 model] makes a function that finds the bound
      that should give the desired number of node expansions. *)
  let get_child_costs parent_costs depth bound =
    let min_parent_g = Hist.min_val parent_costs in
    let c_hist = Hist.make model.max_bins in
      for r = 0 to model.max_children - 1 do
	let c = model.cost_fun depth r in
	  Verb.pr Verb.debug "Adding mass 1.0 at c=%f\n" c;
	  if min_parent_g +. c <= bound then Hist.add_mass c 1.0 c_hist;
      done;
      c_hist
  in
    (fun desired ->
       Blfs_bound.find_bound
	 ~max_bins:model.max_bins
	 (branch_prob model)
	 get_child_costs
	 ~root_cost:0.
	 ~desired)


let search
    leaf_cost
    max_depth
    max_children
    cost_fun
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
  (** [search leaf_cost max_depth max_children ?max_bins ?num_probes
      ?is_optimal ?should_prune ?prev_best ?halt ?log copy_state
      is_better is_leaf num_children nth_child initial] performs a
      BLFS search using the quadratic regression model. *)
  let info =
    Info.make num_children nth_child is_leaf is_better is_optimal
      copy_state should_prune log prev_best halt
  in
  let model = make cost_fun max_bins max_children max_depth in
  let initial_data = make_initial_data model
  and see_branch = make_see_branch info model
  and see_terminal = make_see_terminal model
  and see_leaf = make_see_leaf leaf_cost model
  and remember_edge = make_remember_edge model
  and forget_edge = make_forget_edge model
  and find_bound = make_find_bound model in
  let best_completion _ _ = 0. in
  let iter_children =
    Blfs_regression.make_iter_children info see_branch
      (fun depth rank _ -> model.cost_fun depth rank)
      best_completion remember_edge forget_edge
  in
  let blfs_interface =
    Blfs.make_interface
      initial_data see_leaf see_terminal iter_children find_bound
  in
    Blfs.best_leaf_first_search blfs_interface info 0. initial

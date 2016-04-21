(** A simple model that tries to mimic ILDS by charging 1 for going
    right and 0 for going left.

    @author eaburns
    @since 2010-01-10
*)

type t = {
  max_bins : int;
  (* Maximum number of histogram bins to use. *)

  tree : Blfs_tree_stats.t;

  mutable bound : float;
  (* The number of discrepancies to allow. *)
}


let make max_bins max_children max_depth =
  (** [make max_bins max_children max_depth] makes a new quadratic
      model. *)
  {
    max_bins = 200;
    tree = Blfs_tree_stats.make ~max_depth ~max_children;

    bound = ~-.1.;
    (* Setting the bound to -1 starts with 0 discrepancies. *)
  }


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
  (fun _ (_, depth) -> Blfs_tree_stats.see_terminal model.tree depth)


let make_remember_edge model =
  (** [make_remember_edge model] makes a function that remembers the
      fact that the search has taken the edge (depth, rank). *)
  (fun depth rank _ -> ())


let make_forget_edge model =
  (** [make_forget_edge model] makes a function that forgets the fact
      that the search has taken the edge (depth, rank). *)
  (fun depth rank _ -> ())


let edge_cost depth rank _ = if rank = 0 then 0. else 1.
  (** [edge_cost depth rank cost] get the cost of an edge. *)


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


(*
let make_find_bound model =
  (** [make_find_bound model] makes a function that finds the bound
      that should give the desired number of node expansions. *)
  let max_bins = model.max_bins in
  let get_child_cost depth rank max_cost =
    let c = edge_cost depth rank in
      if c <= max_cost
      then
	let p = Blfs_tree_stats.p_with_child_rank model.tree ~depth ~rank in
	  Some (Blfs_bound_old.init_hist_with max_bins c p)
      else
	None
  in
  let get_child_h depth rank = None
  in
    (fun desired ->
       Blfs_bound_old.find_bound_for_nodes
	 model.tree.Blfs_tree_stats.max_depth
	 model.tree.Blfs_tree_stats.max_children
	 model.max_bins
	 0.				(* root_cost *)
	 0.				(* root_f *)
	 ~get_child_cost
	 ~get_child_h
	 model.tree.Blfs_tree_stats.non_terms
	 model.tree.Blfs_tree_stats.terms
	 desired)
*)


(*
let make_find_bound model =
  (** [make_find_bound model] makes a function that finds the bound
      that should give the desired number of node expansions. *)
  let get_child_hists depth bound =
    let c_hist = Hist.make model.max_bins
    and ch_hist = Hist.make model.max_bins in
      for r = 0 to model.tree.Blfs_tree_stats.max_children - 1 do
	let c = edge_cost depth r in
	let p = Blfs_tree_stats.p_with_child_rank model.tree depth r in
	  if c < bound
	  then begin
	    Hist.add_mass c p c_hist;
	    Hist.add_mass c p ch_hist;
	  end
      done;
      c_hist, ch_hist
  in
    (fun desired ->
       Blfs_bound.find_bound
	 ~max_bins:model.max_bins
	 (Blfs_tree_stats.branch_prob model.tree)
	 get_child_hists
	 ~root_g:0.
	 ~desired)
*)

let make_find_bound model =
  (fun desired ->
     model.bound <- model.bound +. 1.;
     model.bound, desired)


let search
    leaf_cost
    max_depth
    max_children
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
  let model = make max_bins max_children max_depth in
  let initial_data = make_initial_data model
  and see_branch = make_see_branch info model
  and see_terminal = make_see_terminal model
  and see_leaf = make_see_leaf leaf_cost model
  and remember_edge = make_remember_edge model
  and forget_edge = make_forget_edge model
  and find_bound = make_find_bound model in
  let best_completion _ _ = 0. in
  let iter_children =
    Blfs_regression.make_iter_children info see_branch edge_cost
      best_completion remember_edge forget_edge
  in
  let blfs_interface =
    Blfs.make_interface
      initial_data see_leaf see_terminal iter_children find_bound
  in
(*
    Blfs_regression.random_probes info max_children see_branch see_leaf
      remember_edge forget_edge initial num_probes;
*)
    Blfs.best_leaf_first_search ~limit:false blfs_interface info 0. initial

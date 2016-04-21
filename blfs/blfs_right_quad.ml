(** A BLFS model that makes left branches free but charges
    quadratically in depth for right branches.

    @author eaburns
    @since 2010-01-12
*)

type t = {
  costs : float array;
  (* An array of the pre-computed cost values.  Computed before each
     iteration. *)

  sums : float array;
  (* Sum depth information for branches going to the right. *)

  mutable gone_right : int;
  (* Number of times the current path has gone right.  This is used
     because the learner does not work properly when the pattern is all
     zeroes. *)

  tree : Blfs_tree_stats.t;
  (* Various statistics on the tree structure: terms, non-terms,
     branching. *)

  max_bins : int;
  (* Maximum number of histogram bins to use. *)

  mutable left_leaf_cost : float;

  show_leaf_cost : float -> unit;
  (* Shows a leaf cost to the learner. *)

  get_coeffs : unit -> float array;
  (* Gets the coefficients from the learner. *)
}


let dd_index rank =
  (** [dd_index rank] gets the index of the d^2 index for the given
      rank. *)
  assert (rank > 0);
  (rank - 1) * 3



let make learner ~max_bins ~max_children ~max_depth =
  (** [make learner ~max_bins ~max_children ~max_depth] makes a new
      model. *)
  let n = (max_children - 1) * 3 in
  let sums = Array.make n 0. in
  let show_learner, _, get_coeffs = learner n in
  let show_leaf_cost diff = ignore (show_learner sums diff) in
    {
      costs = Array.make ((max_depth + 1) * max_children) 0.;
      sums = sums;
      gone_right = 0;

      tree = Blfs_tree_stats.make ~max_children ~max_depth;
      max_bins = 200;

      left_leaf_cost = ~-.1.;
      (* Assumes there are no negative cost leafs *)

      show_leaf_cost = show_leaf_cost;
      get_coeffs = get_coeffs;
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
  (fun node (cost, depth) ->
     Blfs_tree_stats.see_terminal model.tree depth;
(*
     Wrutils.pr "leaf cost=%f, gone_right=%d\n" cost model.gone_right;
*)
     let cost = leaf_cost node in
       if model.left_leaf_cost = ~-.1.
       then model.left_leaf_cost <- cost
       else begin
	 if model.gone_right > 0
	 then model.show_leaf_cost (cost -. model.left_leaf_cost)
       end)


let lookup_edge_cost model ~depth ~rank =
  (** [lookup_edge_cost model ~depth ~rank] looks up the edge cost in
      the table. *)
  model.costs.(model.tree.Blfs_tree_stats.max_children * depth + rank)



let get_cost model ~rank ~depth =
  (** [get_cost model rank depth] computes the cost of an
      edge. *)
  let init_c = if depth = 0 then model.left_leaf_cost else 0. in
    init_c +. (lookup_edge_cost model ~depth ~rank)



let make_remember_edge model =
  (** [make_remember_edge model] makes a function that remembers the
      fact that the search has taken the edge (depth, rank). *)
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
    (fun depth rank _ ->
       if rank > 0
       then begin
	 (*
	   Wrutils.pr "depth=%d Going right for a cost of %f\n"
	   depth
	   (edge_cost model ~rank ~depth);
	 *)
	 model.gone_right <- model.gone_right + 1;
	 let d = (float depth) /. max_depthf in
	 let dd = d *. d in
	 let ddi = dd_index rank in
	 let di = ddi + 1 in
	 let ci = di + 1 in
	   model.sums.(ddi) <- model.sums.(ddi) +. dd;
	   model.sums.(di) <- model.sums.(di) +. d;
	   model.sums.(ci) <- model.sums.(ci) +. 1.0;
       end)


let make_forget_edge model =
  (** [make_forget_edge model] makes a function that forgets the fact
      that the search has taken the edge (depth, rank). *)
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
    (fun depth rank _ ->
       if rank > 0
       then begin
	 model.gone_right <- model.gone_right - 1;
	 let d = (float depth) /. max_depthf in
	 let dd = d *. d in
	 let ddi = dd_index rank in
	 let di = ddi + 1 in
	 let ci = di + 1 in
	   model.sums.(ddi) <- model.sums.(ddi) -. dd;
	   model.sums.(di) <- model.sums.(di) -. d;
	   model.sums.(ci) <- model.sums.(ci) -. 1.0;
       end)


let  fill_cost_table model =
  (** [fill_cost_table model] fills in the cost table for the model.
      Makes each 0th ranked child free but costs are not guaranteed to be
      monotonically increasing in rank. *)
  let coeffs = model.get_coeffs ()
  and costs = model.costs in
  let max_depth = model.tree.Blfs_tree_stats.max_depth
  and max_children = model.tree.Blfs_tree_stats.max_children in
  let max_depthf = float max_depth in
    for depth = 0 to max_depth do
      let base = depth * max_children
      and d = (float depth) /. max_depthf in
      let dd = d *. d in
	costs.(base) <- 0.;
	for rank = 1 to max_children - 1 do
	  let ddi = dd_index rank in
	  let di = ddi + 1
	  and ci = ddi + 2 in
	  let a = coeffs.(ddi)
	  and b = coeffs.(di)
	  and c = coeffs.(ci)
	  in
	    costs.(base + rank) <- a *. dd +. b *. d +. c;
	    if costs.(base + rank) < 0.
	    then costs.(base + rank) <- 0.;
	done
    done;
    Blfs_regression.clamp_monotonic costs
      model.tree.Blfs_tree_stats.max_depth
      model.tree.Blfs_tree_stats.max_children
(*
    Blfs_regression.normalize_for_zero_completions costs
      model.tree.Blfs_tree_stats.max_depth
      model.tree.Blfs_tree_stats.max_children
*)


let prepare_for_search model =
  (** [prepare_for_search model] prepares the model for another
      iteration of search. *)
  model.gone_right <- 0;
  Wrarray.fill_all model.sums 0.;
  Blfs_tree_stats.clear model.tree


let make_initial_data model =
  (** [make_initial_data model] makes a function that gets the root
      node for the beginning of an iteration of BLFS search. *)
  (fun _ ->
     prepare_for_search model;
     0., 0)


let make_find_bound0 model nodes =
  let get_child_cost depth rank max_cost =
    let g = lookup_edge_cost model ~depth ~rank in
      if g <= max_cost
      then begin
	let p = Blfs_tree_stats.p_with_child_rank model.tree depth rank in
	  Some (Blfs_bound_old.init_hist_with model.max_bins g p)
      end else None
  in
    fill_cost_table model;
    Blfs_bound_old.find_bound_for_nodes
      model.tree.Blfs_tree_stats.max_depth
      model.tree.Blfs_tree_stats.max_children
      model.max_bins
      0.
      0.
      ~get_child_cost
      model.tree.Blfs_tree_stats.non_terms
      model.tree.Blfs_tree_stats.terms
      nodes


let make_find_bound model =
  (** [make_find_bound model] makes a function that finds the bound
      that should give the desired number of node expansions. *)
  let get_child_costs parent_costs depth bound =
    let min_parent_g = Hist.min_val parent_costs in
    let c_hist = Hist.make model.max_bins in
      for r = 0 to model.tree.Blfs_tree_stats.max_children - 1 do
	let c = lookup_edge_cost model ~depth ~rank:r in
	let p = Blfs_tree_stats.p_with_child_rank model.tree depth r in
	  if min_parent_g +. c <= bound then Hist.add_mass c p c_hist;
      done;
      c_hist
  in
    (fun desired ->
       fill_cost_table model;
       Blfs_bound.find_bound
	 ~max_bins:model.max_bins
	 (Blfs_tree_stats.branch_prob model.tree)
	 get_child_costs
	 ~root_cost:model.left_leaf_cost
	 ~desired)


let search
    leaf_cost
    max_depth
    max_children
    ?(learner_name = "lms")
    ?(learning_rate = 0.001)
    ?(max_bins = 20000)
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
      (fun depth rank _ -> get_cost model ~rank ~depth) (* edge cost *)
      (fun _ _ -> 0.)			(* best completion *)
      remember_edge
      forget_edge
  in
  let blfs_interface =
    Blfs.make_interface
      initial_data see_leaf see_terminal iter_children find_bound
  in
    (* Shoot down the left branch. *)
    Blfs_regression.always_take_rank info see_branch see_leaf
      see_terminal remember_edge forget_edge initial 0;
    assert (model.left_leaf_cost > 0.);
    Blfs_regression.random_probes info max_children see_branch
      see_leaf see_terminal remember_edge forget_edge initial num_probes;
    Blfs.best_leaf_first_search blfs_interface info 0. initial

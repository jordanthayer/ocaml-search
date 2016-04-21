(* $Id: indecision_max.ml,v 1.2 2004/07/08 22:41:38 ruml Exp ruml $

   the max version of the indecision model for BLFS

   cost of leaf is *MAX* along path.

   the "data" that we pass down is the current depth.
*)


open Indecision


(*********** updating model ************)


let initial_data m =
  Indecision.prepare_for_search m;
  0


let update_terminal m depth =
  Blfs_tree_stats.see_terminal m.tree depth


let map_children m get_costs =
  (fun do_child notify_skipping n depth bound ->
     let raw_costs = get_costs n in
     let costs = get_indecision false raw_costs in
     let nchildren = List.length raw_costs in
       Blfs_tree_stats.see_branch m.tree ~depth ~nchildren;
       Indecision.record_costs m depth costs;
       let next = depth + 1 in
	 do_child n 0 next;
	 Wrlist.iteri (fun i c ->
			 if c <= bound
			 then do_child n (i+1) next
			 else notify_skipping ())
	   costs)


(************* estimating a cost bound *****************)


let bound_for_nodes m nodes =
  (** we are using the max model, so we DON'T want additive
      convolution of the current nodes with the children.  We merely add
      the children in, maxing as we go.  We still always have a zero-cost
      continuation.  The same pruning techniques as with separate can be
      used. *)
    (*
  let get_child_cost depth child max_cost =
    let h = m.costs.(depth).(child) in
      if Hist.has_mass h then
	let min = Hist.min_val h in
	  if min <= max_cost then
	    (* prune weight that we know won't be used *)
	    (Hist.prune_value_right max_cost h;
	     Some h)
	  else None
      else None
  in
      Blfs_bound_old.find_bound_for_nodes (max_depth m)
      (max_children m) (max_bins m)
      0. 0. ~get_child_cost
      m.tree.Blfs_tree_stats.non_terms
      m.tree.Blfs_tree_stats.terms
      ~convolve:Hist.convolve_max_pruning
      nodes
    *)
  let get_child_costs parent_costs depth bound =
    let indecisions = m.costs.(depth) in
    let max_cost = bound -. (Hist.min_val parent_costs) in
    let max_children = max_children m in
    let c_hists = ref [] in
      for r = 0 to max_children - 1 do
	let indecision = indecisions.(r) in
	let p = Blfs_tree_stats.p_with_child_rank m.tree ~depth ~rank:r in
	  Hist.normalize p indecision;
	  c_hists := indecision :: !c_hists;
      done;
      let costs = Hist.add !c_hists in
	Hist.prune_value_right max_cost costs;
	costs
  in
  let branch_prob = Blfs_tree_stats.branch_prob m.tree in
    Blfs_bound.find_bound ~convolve:Hist.convolve_max_pruning
      ~max_bins:(max_bins m) branch_prob get_child_costs
      ~root_cost:0. ~desired:nodes


(********** packaging up the interface *************)


let make_blfs_interface max_depth max_children max_bins get_child_costs =
  let m = Indecision.make max_depth max_children max_bins in
  let init _ = initial_data m
  and update _ = update_terminal m
  and map = map_children m get_child_costs
  and bound = bound_for_nodes m
  in
    Blfs.make_interface init update update map bound


(************* integration with BLFS *****************)


let max_indecision_search max_depth max_children get_child_costs
  ?(max_bins = 200)
  ?(optimal_p = (Fn.constantly1 false))
  ?(prune_p = (Fn.constantly2 false))
  ?(prev_best = None)
  ?(halt = [Info.Never])
  ?(log = Fn.no_op2)
  copy_state
  better_p
  leaf_p
  num_children
  get_child
  initial
  =
  let info = Info.make num_children get_child leaf_p better_p optimal_p
               copy_state prune_p log prev_best halt
  and model = make_blfs_interface max_depth max_children max_bins
		get_child_costs in
  let complete = (if (Info.optimal_so_far info)
		  then (Info.leaf_or_prune_p info initial)
		  else Blfs.bounded_dfs model info 0. initial) in
    if complete || (Info.optimal_so_far info) then
      (Info.curr_best info), (Info.stats info), true, complete
    else
      Blfs.best_leaf_first_search model info 0. initial


(* EOF *)

(* $Id: indecision.ml,v 1.1 2003/07/18 21:21:43 ruml Exp ruml $

   the summation version of the indecision model for BLFS

   cost of leaf is *SUM* along path.
   cost of intermediate child is the amount by which its heuristic score
      is greater than that of its best sibling.

   the "data" that we pass down is the current cost and the current depth.
*)

open Fn

(**
 * Model used by indecision search to record branching probability and
 * cost distributions
 *)
type t = {
  (* 2D array of cost histograms. 1st index is depth, 2nd is child rank *)
  costs : Hist.t array array;
  (* array of leaf counts where the index is the depth of the leaf *)
  tree : Blfs_tree_stats.t;
}

let max_depth m = m.tree.Blfs_tree_stats.max_depth
(** Returns the max depth represented in the model *)


let max_children m = m.tree.Blfs_tree_stats.max_children
(** Returns the max children of a node, as represented in the model *)


let max_bins m = Hist.max_pts m.costs.(0).(0)
(** Returns the max bins used by the histograms of the model *)


(*********** initial model *************)

let make_child_costs mb max_c =
(** Makes an array of histigrams to be used to record child costs.

    mb = max bins to use in histogram

    max_c = max number of children (max branching factor) *)
  Array.init max_c (fun _ -> Hist.make mb)


let make max_d max_c max_b =
  (** Make the model.

      max_d = max depth

      max_c = max children per node

      max_b = max points per cost histogram *)
  (* only need to create one child array, as model will be immediately
     reinitialized by initial_data *)
  {
    costs = Array.make max_d (make_child_costs max_b max_c);
    tree = Blfs_tree_stats.make ~max_children:max_c ~max_depth:max_d
  }


(*********** updating model ************)


let prepare_for_search m =
  (** Set all counts back to 0, re-initialize histograms. *)
  Blfs_tree_stats.clear m.tree;
  for d = 0 to (max_depth m) - 1 do
    m.costs.(d) <- make_child_costs (max_bins m) (max_children m);
    (* first child is always 0. *)
    Hist.add_mass 0. 1. m.costs.(d).(0)
  done


let initial_data m =
  (** Initialize data to 0. Data needed is cost so far and depth. *)
  prepare_for_search m;
  0.,0


let update_prune m (_, depth) =
  (* depth is the depth we are coming *from*, so deepest leaf's count
     is at max_depth - 1 *)
  Blfs_tree_stats.see_terminal m.tree depth


let update_leaf m info leaf_cost node (indecision, depth) =
  Blfs_tree_stats.see_terminal m.tree depth;
  if Verb.level Verb.toplvl
  then Verb.pr Verb.always "(%f, %f);\n" (leaf_cost node) indecision


let norm_ind best vl =
  assert (if best < 0. then vl < 0. else vl >= 0.);
  let fact = (if best < 0. then
		abs_float (best /. vl)
	      else
		abs_float (vl /. best))
  in fact -. 1.


let get_indecision norm = function
    (** Remove the first cost from the list, since it will always be
	0. Subtract the first cost from all the others, or normalize
	the costs to the fraction of the best cost. *)
  | [] -> failwith "get_indecision: no costs"
  | best::others ->
      if norm then
	List.map (norm_ind best) others
      else
	List.map (fun o -> o -. best) others


(** Add costs to histograms for each depth and child rank

    depth = depth we're at

    costs = ordered list of chlid costs *)
let record_costs m depth costs =
  let hists = m.costs in
    (* mass for rank 0 is added in prepare search. *)
    Wrlist.iteri (fun i cost ->
		    (* first cost has been removed, so bump index *)
		    Hist.add_mass cost 1. hists.(depth).(i+1))
      costs


(** Requires the model being used and a function to get child costs
    of a node.  Returns a function which iterates over the children
    of a node.

    @param m is the model

    @param get_costs is a function which takes a node and returns a
    sorted list of child costs *)
let iter_children_bottom_first norm indecision_wt m get_costs =
  (fun visit_child notify_skipping n (so_far, depth) bound ->
     let raw_costs = get_costs n in
     let nchildren = List.length raw_costs in
     let costs = get_indecision norm raw_costs in
     let wt = indecision_wt depth in
       record_costs m depth costs;
       Blfs_tree_stats.see_branch m.tree ~depth ~nchildren;
       let next = depth + 1 in
         (* visit 0 cost child, then iterate over remaining children *)
         visit_child n 0 (so_far, next);
         Wrlist.iteri (fun i c ->
			 let c = c *. wt in
                         let this = c +. so_far in
                           if this <= bound
			   then visit_child n (i+1) (this, next)
                           else notify_skipping ())
           costs)


(** Requires the model being used and a function to get child costs
    of a node.  Returns a function which iterates over the children
    of a node.

    @param m is the model

    @param get_costs is a function which takes a node and returns a
    sorted list of child costs *)
let iter_children_top_first norm indecision_wt m get_costs =
  (fun visit_child notify_skipping n (so_far, depth) bound ->
     let raw_costs = get_costs n in
     let nchildren = List.length raw_costs in
     let costs = get_indecision norm raw_costs in
     let wt = indecision_wt depth in
       record_costs m depth costs;
       Blfs_tree_stats.see_branch m.tree ~depth ~nchildren;
       let next = depth + 1 in
         (* visit 0 cost child, then iterate over remaining children *)
         visit_child n 0 (so_far, next);
         Wrlist.iteri (fun i c ->
			 let c = c *. wt in
                         let this = c +. so_far in
                           if this <= bound
			   then visit_child n (nchildren - (i+1)) (this, next)
                           else notify_skipping ())
           (List.rev costs))


(************* estimating a cost bound *****************)

let bound_for_nodes_sum model =
  (** [bound_for_nodes_sum model] makes a function that finds the bound
      that should give the desired number of node expansions. *)
  let max_children = max_children model in
  let branch_prob = Blfs_tree_stats.branch_prob model.tree in
  let max_bins = max_bins model in
  let costs = model.costs in
    (* Get the edge costs for these children. *)
  let get_child_costs parent_costs depth bound =
    let indecisions = costs.(depth) in
    let max_cost = bound -. (Hist.min_val parent_costs) in
    let c_hists = ref [] in
      for r = 0 to max_children - 1 do
	let indecision = indecisions.(r) in
	let p = Blfs_tree_stats.p_with_child_rank model.tree ~depth ~rank:r in
	  Hist.normalize p indecision;
	  c_hists := indecision :: !c_hists;
      done;
      let costs = Hist.add !c_hists in
	Hist.prune_value_right max_cost costs;
	costs
  in
    (fun desired ->
       Blfs_bound.find_bound branch_prob get_child_costs ~max_bins
	 ~root_cost:0. ~desired)


let bound_for_nodes = bound_for_nodes_sum


(********** packaging up the interface *************)


(* get_costs needs to be hidden internally since not all blfs models
   use child costs *)

let make_blfs_interface
    norm indecision_wt bottom_first info leaf_cost max_depth max_children
    max_bins get_child_costs =
  let m = make max_depth max_children max_bins in
  let init _ = initial_data m
  and update_prune _ = update_prune m
  and update_leaf = update_leaf m info leaf_cost
  and iter =
    if bottom_first
    then iter_children_bottom_first norm indecision_wt m get_child_costs
    else iter_children_top_first norm indecision_wt m get_child_costs
  and bound = bound_for_nodes m
  in
    (Blfs.make_interface init update_leaf update_prune iter bound), m


(************* integration with BLFS *****************)


let indecision_search max_depth max_children get_child_costs
    ?(indecision_wt=constantly1 1.)
    ?(norm=false)
    ?(leaf_cost=Fn.constantly1 nan)
    ?(max_bins = 200)
    ?(optimal_p = (Fn.constantly1 false))
    ?(prune_p = (Fn.constantly2 false))
    ?(prev_best = None)
    ?(halt = [Info.Never])
    ?(log = Fn.no_op2)
    ?(bottom_first = true)
    copy_state
    better_p
    leaf_p
    num_children
    get_child
    initial
    =

  let info = Info.make num_children get_child leaf_p better_p optimal_p
    copy_state prune_p log prev_best halt in
  let model, m =
    make_blfs_interface norm indecision_wt bottom_first info leaf_cost
      max_depth max_children max_bins get_child_costs in
  let complete = (if (Info.optimal_so_far info)
		  then (Info.leaf_or_prune_p info initial)
		    (* do zero-cost leaves *)
		  else Blfs.bounded_dfs model info 0. initial)
  in
  let finished = complete || (Info.optimal_so_far info) in
    if finished then
      ((Info.curr_best info), (Info.stats info), (Info.optimal_so_far info),
       complete)
    else
      Blfs.best_leaf_first_search model info 0. initial


(* EOF *)

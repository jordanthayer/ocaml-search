(** Indecision search with controlled-reexpansion.

    This is based on indecision.ml (eaburns 2011-01-11)

*)

open Fn

type t = {
  bins : float array;
  nbins : int;
  mutable n_oob : int;
  mutable last_bin : int;
  mutable bound : float;
  mutable min_out_of_bound : float;
  tree : Blfs_tree_stats.t;
  descendants : float array;
}

let max_depth m = m.tree.Blfs_tree_stats.max_depth
(** Returns the max depth represented in the model *)


let max_children m = m.tree.Blfs_tree_stats.max_children
(** Returns the max children of a node, as represented in the model *)


let make max_d max_c nbins = {
  bins = Array.make nbins 0.;
  nbins = nbins;
  n_oob = 0;
  last_bin = 1;
  bound = nan;
  min_out_of_bound = infinity;
  tree = Blfs_tree_stats.make ~max_children:max_c ~max_depth:max_d;
  descendants = Array.create (max_d + 1) 0.;
}

let reset_model model =
  let bs = model.bins in
    Blfs_tree_stats.clear model.tree;
    model.bound <- nan;
    model.last_bin <- 1;
    model.min_out_of_bound <- infinity;
    model.n_oob <- 0;
    for i = 0 to model.nbins - 1 do bs.(i) <- 0. done


(** Initialize data to 0. Data needed is cost so far and depth. *)
let initial_data m =
  let max = max_depth m in
  let descendants = m.descendants in
    for d = max downto 0 do
      let nxt = if d = max then 0. else descendants.(d + 1) in
      let p = Blfs_tree_stats.p_with_child_rank m.tree ~depth:d ~rank:0 in
	descendants.(d) <- p *. (1. +. nxt);
    done;
    reset_model m;
    0.,0


let update_prune m (_, depth) =
  (* depth is the depth we are coming *from*, so deepest leaf's count
     is at max_depth - 1 *)
  Blfs_tree_stats.see_terminal m.tree depth


let update_leaf m info leaf_cost node (indecision, depth) =
  Blfs_tree_stats.see_terminal m.tree depth;
  if Verb.level Verb.toplvl then
    Verb.pr Verb.always "(%f, %f);\n" (leaf_cost node) indecision


(** Remove the first cost from the list, since it will always be
    0. Subtract the first cost from all the others, or normalize
    the costs to the fraction of the best cost. *)
let get_indecision = function
  | [] -> failwith "get_indecision: no costs"
  | best :: rest -> List.map (fun o -> o -. best) rest


(** Get the appropriate bin as described in the IDA*_CR paper. *)
let get_bin nbins bound f =
  (* This should find i s.t:

     x(1 + i/100) < f(n) <= x(1 + (i + 1)/100), where x is the
     bound. *)
  let rec do_get_bin i =
    if i > nbins
    then nbins
    else
      let p = (float i) in
	if bound *. (1. +. p /. 100.) >= f
	then if i = 1 then i else i - 1
	else do_get_bin (i + 1)
  in do_get_bin 1


(** Get the p-value that will give the desired weight.  If there is
    not enough weight then return [list_bin].*)
let get_p_for_wt last_bin bins wt =
  let n = Array.length bins in
  let rec do_get_p accum p =
    if p > n then
      last_bin
    else begin
      assert (p > 0);
      let sum = accum +. bins.(p - 1) in
	if sum >= wt then
	  p
	else
	  do_get_p sum (p + 1)
    end
  in do_get_p 0. 1


(** Add the child costs to our model if it is out of the bound. *)
let record_costs m ~depth ~bound ~so_far edge_costs =
  let bins = m.bins in
  let nbins = m.nbins in
  let wt = 1. +. m.descendants.(depth) in
    List.iter (fun edge_cost ->
		 let cost = so_far +. edge_cost in
		   if cost > bound then begin
		     m.n_oob <- m.n_oob + 1;
		     if m.min_out_of_bound > cost then
		       m.min_out_of_bound <- cost;
		     let i = get_bin nbins bound cost in
		       if (i - 1) < nbins then begin
			 assert (i > 0);
			 bins.(i - 1) <- bins.(i - 1) +. wt;
			 m.last_bin <- max m.last_bin i;
		       end
		   end)
      edge_costs



(** Requires the model being used and a function to
    get child costs of a node.  Returns a function
    which iterates over the children of a node.

    @param m is the model

    @param get_costs is a function which takes a node and returns a
    sorted list of child costs *)
let iter_children_bottom_first m get_costs =
  (fun visit_child notify_skipping n (so_far, depth) bound ->
     let raw_costs = get_costs n in
     let costs = get_indecision raw_costs in
     let nchildren = List.length raw_costs in
       if Math.nan_p m.bound then m.bound <- bound;
       (* it is OK to not record the 0th rank cost because that will
	  always go to a leaf anyway. *)
       record_costs m ~depth ~bound ~so_far costs;
       Blfs_tree_stats.see_branch m.tree ~depth ~nchildren;
       let next = depth + 1 in
	 (* visit 0 cost child, then iterate over remaining children *)
	 visit_child n 0 (so_far, next);
	 Wrlist.iteri (fun i c ->
			 let this = c +. so_far in
			   if this <= bound then
			     visit_child n (i+1) (this, next)
			   else
			     notify_skipping ())
	   costs)


(** Requires the model being used and a function to get child costs
    of a node.  Returns a function which iterates over the children
    of a node.

    @param m is the model

    @param get_costs is a function which takes a node and returns a
    sorted list of child costs *)
let iter_children_top_first m get_costs =
  (fun visit_child notify_skipping n (so_far, depth) bound ->
     let raw_costs = get_costs n in
     let nchildren = List.length raw_costs in
     let costs = get_indecision raw_costs in
       if Math.nan_p m.bound then m.bound <- bound;
       (* it is OK to not record the 0th rank cost because that will
	  always go to a leaf anyway. *)
       record_costs m ~depth ~bound ~so_far costs;
       Blfs_tree_stats.see_branch m.tree ~depth ~nchildren;
       let next = depth + 1 in
         (* visit 0 cost child, then iterate over remaining children *)
         visit_child n 0 (so_far, next);
         Wrlist.iteri (fun i c ->
                         let this = c +. so_far in
                           if this <= bound then
			     visit_child n (nchildren - (i+1)) (this, next)
                           else
			     notify_skipping ())
           (List.rev costs))



(** Estimates a cost bound for a desired number of nodes *)
let bound_for_nodes m desired =
  let j = float (get_p_for_wt m.last_bin m.bins (float desired)) in
  let proposed = m.bound *. (1. +. j /. 100.) in
  let bound' = Math.fmax m.min_out_of_bound proposed in
    Verb.pr Verb.optional "proposed=%g, min_oob=%g, bound'=%g\n"
      proposed m.min_out_of_bound bound';
    Verb.pr Verb.optional "n_oob=%d\n" m.n_oob;
    reset_model m;
    bound', desired


(********** packaging up the interface *************)


(* get_costs needs to be hidden internally since not all blfs models
   use child costs *)

let make_blfs_interface max_d max_c bottom_first info leaf_cost
    max_bins get_child_costs =
  let m = make max_d max_c max_bins in
  let init _ = initial_data m in
  let update_prune _ = update_prune m in
  let update_leaf = update_leaf m info leaf_cost in
  let iter =
    if bottom_first then
      iter_children_bottom_first m get_child_costs
    else
      iter_children_top_first m get_child_costs
  in
  let next_bound = bound_for_nodes m in
    (Blfs.make_interface init update_leaf update_prune iter next_bound), m


(************* integration with BLFS *****************)


let indecision_search max_depth max_children get_child_costs
    ?(leaf_cost=Fn.constantly1 nan)
    ?(max_bins = 100)
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
    make_blfs_interface max_depth max_children bottom_first
      info leaf_cost max_bins get_child_costs in
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

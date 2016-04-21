(** A BLFS weighted indecision model using quadratics.

    @author eaburns
    @since 2010-01-12
*)

type t = {
  coeffs : float array;
  (* An array of the coefficients for the learner. *)

  indecisions : Hist.t array array;
  (* 2D array of indecision histograms. 1st index is depth, 2nd is child
     rank *)

  costs: float array;
  (* Cost of a branch at each depth (this doesn't include the
     indecision... that must be added on). *)

  sums : float array;
  (* Sums of left, right and indecision during each level in the
     search. *)

  tree : Blfs_tree_stats.t;
  (* Various statistics on the tree structure: terms, non-terms,
     branching. *)

  mutable root_cost : float;
  (* The cost of the root node. *)

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

let nfeatures = 9
  (* 3 quadratics: left branches, right branches, indecisions. *)

let degree = 2
  (* Use a quadratic polynomial. *)

(** {6 Initialization} ****************************************)

let make ?init_model learner ~max_bins ~max_children ~max_depth =
  (** [make learner ~max_bins ~max_children ~max_depth] makes a new
      model. *)
  let n = nfeatures in
  let sums = Array.make n 0. in
  let show_learner, _, get_coeffs = learner n in
  let accum_dev, output_dev = Blfs_regression.make_error_tracker "" in
  let coeffs = get_coeffs () in
    Array.iteri (fun i _ -> coeffs.(i) <- 1.) coeffs;
    let show_leaf_cost = match init_model with
      | Some a ->
	  let len = Array.length a in
	    assert (len = n);
	    Array.blit a 0 coeffs 0 n;
	    (*
	      (fun cost -> nan)
	    *)
	    (fun cost -> let _, err = show_learner sums cost in err)
      | None ->
	  (fun cost -> let _, err = show_learner sums cost in err)
    in
      {
	coeffs = Array.make n 1.;
	indecisions =
	  Array.make max_depth
	    (Array.init max_children (fun _ -> Hist.make max_bins));
	costs = Array.make (max_depth * max_children) 0.;
	sums = sums;
	tree = Blfs_tree_stats.make ~max_children ~max_depth;
	accumulate_deviation = accum_dev;
	output_average_deviation = (fun () -> ignore (output_dev ()));
	root_cost = 0.;
	max_bins = max_bins;
	show_leaf_cost = show_leaf_cost;
	get_coeffs = get_coeffs;
      }


let plot_model model () =
  (** [plot_model model ()] makes a plot of the separate model. *)
()
(*
  let weights = model.get_coeffs () in
  let max_depth = model.tree.Blfs_tree_stats.max_depth
  and max_children = model.tree.Blfs_tree_stats.max_children in
    Blfs_plot_model.plot_separate
      ~init_depth:1 max_depth max_children weights "separate"
*)

let prepare_for_search model =
  (** [prepare_for_search model] prepares the model for another
      iteration of search. *)
  (* prepare model.weights before finding the bound (which happens
     before this). *)
  let max_depth = model.tree.Blfs_tree_stats.max_depth in
    model.output_average_deviation ();
    Wrarray.fill_all model.sums 0.;
    Blfs_tree_stats.clear model.tree;
    for d = 0 to max_depth - 1 do
      model.indecisions.(d) <-
	Array.init model.tree.Blfs_tree_stats.max_children
	(fun _ -> Hist.make model.max_bins);
      (* first child is always 0. *)
      Hist.add_mass 0. 1. model.indecisions.(d).(0);
    done


let make_initial_data model =
  (** [make_initial_data model] makes a function that gets the root
      node for the beginning of an iteration of BLFS search. *)
  (fun _ ->
       prepare_for_search model;
       model.root_cost, 0)


(** {6 Expanding} ****************************************)


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
     let lc = leaf_cost node in
     let err = abs_float (model.show_leaf_cost lc) in
       model.accumulate_deviation err;)


let remember_edge model depth rank indecision =
  (** [remember_edge model depth rank indecision] remembers the
      fact that the search has taken the edge (depth, rank). *)
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
  let pdepth = (float depth) /. max_depthf in
  let pdepth2 = pdepth *. pdepth in
  let left = if rank = 0 then 1. else 0.
  and right = if rank <> 0 then 1. else 0. in
  let sums = model.sums in
    sums.(0) <- sums.(0) +. left;
    sums.(1) <- sums.(1) +. pdepth *. left;
    sums.(2) <- sums.(2) +. pdepth2 *. left;
    sums.(3) <- sums.(3) +. right;
    sums.(4) <- sums.(4) +. pdepth *. right;
    sums.(5) <- sums.(5) +. pdepth2 *. right;
    sums.(6) <- sums.(6) +. indecision;
    sums.(7) <- sums.(8) +. pdepth *. indecision;
    sums.(8) <- sums.(8) +. pdepth2 *. indecision


let forget_edge model depth rank indecision =
  (** [forget_edge model depth rank indecision] forgets the fact
      that the search has taken the edge (depth, rank). *)
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
  let pdepth = (float depth) /. max_depthf in
  let pdepth2 = pdepth *. pdepth in
  let left = if rank = 0 then 1. else 0.
  and right = if rank <> 0 then 1. else 0. in
  let sums = model.sums in
    sums.(0) <- sums.(0) -. left;
    sums.(1) <- sums.(1) -. pdepth *. left;
    sums.(2) <- sums.(2) -. pdepth2 *. left;
    sums.(3) <- sums.(3) -. right;
    sums.(4) <- sums.(4) -. pdepth *. right;
    sums.(5) <- sums.(5) -. pdepth2 *. right;
    sums.(6) <- sums.(6) -. indecision;
    sums.(7) <- sums.(8) -. pdepth *. indecision;
    sums.(8) <- sums.(8) -. pdepth2 *. indecision


let branch_cost model depth rank =
  (** [branch_cost model depth rank] computes the branch cost, which
      is the edge cost when not counting the indecision. *)
  let coeffs = model.coeffs in
  let left = if rank = 0 then 1. else 0.
  and right = if rank <> 0 then 1. else 0. in
  let pdepth = (float depth) /. (float model.tree.Blfs_tree_stats.max_depth) in
  let pdepth2 = pdepth *. pdepth in
    (coeffs.(0) *. left)
    +. (coeffs.(1) *. pdepth *. left)
    +. (coeffs.(2) *. pdepth2 *. left)
    +. (coeffs.(3) *. right)
    +. (coeffs.(4) *. pdepth *. right)
    +. (coeffs.(5) *. pdepth2 *. right)


let compute_indecision model depth costs =
  (** [compute_indecision model depth costs] gets an array of indecision values
      from the list of child costs. *)
  match costs with
    | [] -> [| |]
    | best :: _ ->
	let ary = Array.of_list costs in
	let hists = model.indecisions.(depth) in
	  Array.iteri (fun rank vl ->
			 let hist = hists.(rank) in
			 let ind = (vl -. best) in
			   Hist.add_mass ind 1.0 hist;
			   ary.(rank) <- ind) ary;
	  ary


let iter_children info model child_costs =
  (** [iter_children info child_costs] creates a function for visiting
      child nodes. *)
  let max_children = model.tree.Blfs_tree_stats.max_children in
  let coeffs = model.coeffs in
  let rec iter_ranks visit_child notify_skip node cost depth bound
      nchildren indecision rank =
    if rank < nchildren
    then begin
      let ind = indecision.(rank) in
      let pdepth =
	(float depth) /. (float model.tree.Blfs_tree_stats.max_depth) in
      let pdepth2 = pdepth *. pdepth in
      let edge_cost =
	model.costs.(depth * max_children + rank)
	+. ((coeffs.(6) +. (coeffs.(7) *. pdepth) +. (coeffs.(8) *. pdepth2))
	    *. ind)
      in
      let h = 0. in
      let g = cost +. edge_cost in
      let f = g +. h in
	if f <= bound
	then begin
	  (* Visit the child *)
	  remember_edge model depth rank ind;
	  visit_child node rank (g, depth + 1);
	  forget_edge model depth rank ind;

	  (* Visit the next child *)
	  iter_ranks visit_child notify_skip node
	    cost depth bound nchildren indecision (rank + 1)
	end else
	  (* Child is out of bounds.  Assume the remaining children
	     are out of bounds too. *)
	  notify_skip ()
    end
  in
    (fun visit_child notify_skip node (cost, depth) bound ->
       make_see_branch info model depth node;
       let nchildren = Info.num_children info node in
       let indecision = compute_indecision model depth (child_costs node) in
	 if nchildren = 0
	 then make_see_terminal model node (cost, depth)
	 else
	   iter_ranks visit_child notify_skip node
	     cost depth bound nchildren indecision 0)


(** {6 Bounds} ****************************************)

let  fill_cost_table model =
  (** [fill_cost_table model] fills in the cost table for the model.
      Makes each 0th ranked child free but costs are not guaranteed to be
      monotonically increasing in rank. *)
  let costs = model.costs in
  let max_depth = model.tree.Blfs_tree_stats.max_depth
  and max_children = model.tree.Blfs_tree_stats.max_children in
    for depth = 0 to max_depth - 1 do
      let base = depth * max_children in
	for rank = 0 to max_children - 1 do
	  costs.(base + rank) <- branch_cost model depth rank;
	done
    done;
    Blfs_regression.clamp_monotonic costs max_depth max_children;
    let rc =
      Blfs_regression.normalize_for_zero_completions
	costs max_depth max_children
    in model.root_cost <- rc


let mult bins scalar hist =
  (** [mult bins scalar hist] multiplies a histogram handling the case
      that the scalar is zero. *)
  if Math.is_zero scalar
  then begin
(*
    let h = Hist.make bins in
      Hist.add_mass 0. (Hist.total_weight hist) h;
      h
*)
    Hist.make bins
  end else begin
    Hist.multiply hist scalar;
    hist
  end


let make_find_bound model =
  (** [make_find_bound model] makes a function that finds the bound
      that should give the desired number of node expansions. *)
  let max_children = model.tree.Blfs_tree_stats.max_children in
  let coeffs = model.coeffs in
  let get_child_costs parent_costs depth bound =
    let min_parent_g = Hist.min_val parent_costs in
    let c_hist = ref (Hist.make model.max_bins) in
    let indecision = model.indecisions.(depth) in
    let base = depth * max_children in
    let pdepth =
      (float depth) /. (float model.tree.Blfs_tree_stats.max_depth) in
    let pdepth2 = pdepth *. pdepth in
      for r = 0 to max_children - 1 do
	let p = Blfs_tree_stats.p_with_child_rank model.tree ~depth ~rank:r in
	let prune_value = bound -. min_parent_g in
	let br_cost = Hist.make model.max_bins in
	let wt = (coeffs.(6)
		  +. (coeffs.(7) *. pdepth)
		  +. (coeffs.(8) *. pdepth2))
	in
	let ind = mult model.max_bins wt indecision.(r) in
	  Hist.add_mass model.costs.(base + r) 1.0 br_cost;
	  let c = Hist.convolve_pruning br_cost ind prune_value in
	    Hist.normalize p c;
	    Hist.prune_value_right prune_value c;
	    if (Hist.has_mass c) then c_hist := Hist.add [c; !c_hist;];
      done;
      !c_hist
  in
    (fun desired ->
       let coeffs = model.get_coeffs () in
	 Wrarray.copy_into coeffs model.coeffs;
	 fill_cost_table model;
	 Blfs_bound.find_bound
	   ~max_bins:model.max_bins
	   (Blfs_tree_stats.branch_prob model.tree)
	   get_child_costs
	   ~root_cost:model.root_cost
	   ~desired)


(** {6 Probing} ****************************************)

exception Halted

let random_child info model child_costs node depth =
  (** [random_child info model child_costs node depth] gets a random
      child of the given node. *)
  let nchildren = Info.num_children info node in
  let rank = Random.int nchildren in
  let indecision = compute_indecision model depth (child_costs node) in
    Info.get_child info node rank, rank, indecision.(rank)


let nth_child info model child_costs node depth rank =
  (** [nth_child info model child_costs node depth rank] gets the
      [rank]th child of a node.  If [rank] is greater than the number
      of children then the right most child is returned instead. *)
  let nchildren = Info.num_children info node in
  let indecision = compute_indecision model depth (child_costs node) in
  let actual_rank = Math.imin rank (nchildren - 1) in
    (Info.get_child info node actual_rank,
     actual_rank,
     indecision.(actual_rank))


let random_probe info model child_costs see_branch see_leaf see_term root =
  (** [random_probe info child_costs see_branch see_leaf see_term
      edge_cost remember_edge forget_edge root] makes a random probe
      into the search space down to a leaf.

      [see_branch] called with the depth and node each time a
      new branch is seen.

      [see_leaf] is called with the node and (0., depth) each time a
      leaf is reached. *)
  let rec visit node depth =
    if Info.halt_p info
    then
      raise Halted
    else begin
      if Info.leaf_p info node
      then begin
	see_leaf node (0., depth);
	ignore (Info.check_best_and_optimal info node)
      end else
	if (Info.num_children info node) = 0
	then see_term node (0., depth)
	else begin
	  Info.incr_branches info;
	  see_branch depth node;
	  let child, rank, indecision =
	    random_child info model child_costs node depth
	  in
	    remember_edge model depth rank indecision;
	    visit child (depth + 1);
	    forget_edge model depth rank indecision;
	end
    end
  in visit root 0


let always_take_rank info model child_costs
    see_branch see_leaf see_term root rank =
  (** [always_take_rank info child_costs see_branch see_leaf see_term
      rank] makes a probe into the search space down to a leaf taking
      the child with [rank] each time.

      [see_branch] called with the depth and node each time a
      new branch is seen.

      [see_leaf] is called with the node and (0., depth) each

      time a leaf is reached. *)
  let rec visit node depth =
    if Info.halt_p info
    then
      raise Halted
    else begin
      if Info.leaf_p info node
      then begin
	see_leaf node (0., depth);
	ignore (Info.check_best_and_optimal info node)
      end else
	if (Info.num_children info node) = 0
	then see_term node (0., depth)
	else begin
	  Info.incr_branches info;
	  see_branch depth node;
	  let child, actual_rank, indecision =
	    nth_child info model child_costs node depth rank
	  in
	    remember_edge model depth rank indecision;
	    visit child (depth + 1);
	    forget_edge model depth rank indecision;
	end
    end
  in visit root 0


let init_rnd = function
    (** [init_rnd seed] initializes the random number to some seed
	value. *)
  | None -> ()
  | Some s ->
      Verb.pr Verb.optional "Blfs_regression: Using seed = %d\n" s;
      Random.init s


let random_probes ?seed info model child_costs max_children see_branch
    see_leaf see_term root num =
  (** [random_probes info model child_costs max_children see_branch
      see_leaf see_term root num] performs [num] random probes into the search
      space and also probes the left-most and right-most leaves. *)
  init_rnd seed;
  try
    (* left-most leaf. *)
    always_take_rank info model child_costs see_branch
      see_leaf see_term root 0;

    (* right-most leaf. *)
    always_take_rank info model child_costs see_branch see_leaf see_term root
      (max_children - 1);

    (* [num] random probes *)
    for n = 1 to num do
      random_probe info model child_costs see_branch see_leaf see_term root
    done;
  with Halted -> ()

(** {6 Search} ****************************************)

let search
    leaf_cost
    max_depth
    max_children
    child_costs
    ?init_model
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
  let model = make ?init_model learner ~max_bins ~max_children ~max_depth in
  let initial_data = make_initial_data model
  and see_branch = make_see_branch info model
  and see_terminal = make_see_terminal model
  and see_leaf = make_see_leaf leaf_cost model
  and find_bound = make_find_bound model in
  let iter_children = iter_children info model child_costs in
  let blfs_interface =
    Blfs.make_interface
      ~dump:(if dump_model then (plot_model model) else (fun () -> ()))
      initial_data see_leaf see_terminal iter_children find_bound
  in
    prepare_for_search model;
    random_probes info model child_costs max_children see_branch
      see_leaf see_terminal initial num_probes;
    Blfs.best_leaf_first_search blfs_interface info 0. initial

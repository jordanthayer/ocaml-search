(** A model that fits a polynomial (in depth and cost) to each child
    rank in order to estimate child cost.

    @author eaburns
    @since 2010-01-08
*)

type t = {
  degree : int;
  (* The degree of the polynomial. *)

  costs : float array;
  (* An array of the pre-computed cost values.  Computed before each
     iteration. *)

  cost_coeffs : float array;
  (* The coefficient of the current cost for the model. *)

  mutable max_cost : float;
  (* The maximum cost achievable by the current costs. *)

  sums : float array;
  (* Sum of the terms for each rank.  These are the 'variables' of the
     model.  See the comment in [const_index] for a description of their
     ordering. *)

  tree : Blfs_tree_stats.t;
  (* Various statistics on the tree structure: terms, non-terms,
     branching. *)

  accumulate_deviation : float -> unit;
  output_average_deviation : unit -> unit;
  (* Tracking average deviation in the learning. *)

  max_bins : int;
  (* Maximum number of histogram bins to use. *)

  show_leaf_cost : float -> float;
  (* Shows a leaf cost to the learner the results is the error. *)

  get_coeffs : unit -> float array;
  (* Gets the coefficients from the learner. *)
}


let cost_index degree rank = (rank - 1) * (degree + 2)
  (** [cost_index degree rank] gets the index of the cost term for the
      given rank.  The indexes are as follows:

      g/g_max, c, d/d_max, (d/d_max)^2, ..., (d/d_max)^n

      where c is the constant, d is depth, d_max is max depth, g is
      the accumulated cost so far and g_max is the maximum cost. *)


let make degree learner max_bins max_children max_depth =
  (** [make degree learner max_bins max_children max_depth]
      makes a new quadratic model. *)
  let n = (max_children - 1) * (degree + 2) + 1 in
  let sums = Array.make n 0. in
  let show_learner, _, get_coeffs = learner n in
  let show_leaf_cost cost =
    let _, err = show_learner sums cost in
      err
  in
  let accum_dev, output_dev = Blfs_regression.make_error_tracker "" in
    {
      degree = degree;
      costs = Array.make (max_depth  * max_children) 0.;
      cost_coeffs = Array.make max_children 0.;
      max_cost = 0.;
      sums = sums;
      tree = Blfs_tree_stats.make ~max_children ~max_depth;
      accumulate_deviation = accum_dev;
      output_average_deviation = (fun () -> ignore (output_dev ()));
      max_bins = max_bins;
      show_leaf_cost = show_leaf_cost;
      get_coeffs = get_coeffs;
    }


let root_cost model =
  (** [root_cost model] gets the cost of the root node. *)
  let coeffs = model.get_coeffs () in
    coeffs.((Array.length coeffs) - 1)


let plot_model model () =
  ()
(*
  (** [plot_model model ()] plots the polynomials. *)
  let max_depth = model.tree.Blfs_tree_stats.max_depth in
  let degree = model.degree in
  let coeffs = model.get_coeffs () in
  let fs = ref [] in
    for r = model.tree.Blfs_tree_stats.max_children - 1 downto 0 do
      let ci = const_index degree r in
      let ary = Array.make (degree + 1) 0. in
	ary.(0) <- coeffs.(ci);
	for d = 1 to degree do
	  ary.(d) <- coeffs.(ci + d)
	done;
	Wrutils.push (Blfs_plot_model.poly ary) fs;
	Wrutils.pr "rank=%d: " r;
	Array.iter (Wrutils.pr "%f ") ary;
	Wrutils.pr "\n";
    done;
    Blfs_plot_model.plot_poly_model
      (Wrutils.str "polynomial deg=%d const=%f" degree (root_cost model))
      max_depth !fs
*)


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


let make_remember_edge model =
  (** [make_remember_edge model] makes a function that remembers the
      fact that the search has taken the edge (depth, rank). *)
  let degree = model.degree in
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
    (fun depth rank cur_cost ->
       if rank > 0
       then begin
	 let d = (float depth) /. max_depthf in
	 let cost = Math.div0 cur_cost model.max_cost in
	 let cur_term = ref d in
	 let ci = cost_index degree rank in
	   model.sums.(ci) <- model.sums.(ci) +. cost;
	   model.sums.(ci + 1) <- model.sums.(ci + 1) +. 1.0;
	   for t = 2 to degree + 1 do
	     let i = ci + t in
	       model.sums.(i) <- model.sums.(i) +. !cur_term;
	       cur_term := !cur_term *. d;
	   done
       end)


let make_forget_edge model =
  (** [make_forget_edge model] makes a function that forgets the fact
      that the search has taken the edge (depth, rank). *)
  let degree = model.degree in
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
    (fun depth rank cur_cost ->
       if rank > 0 then begin
	 let d = (float depth) /. max_depthf in
	 let cost = Math.div0 cur_cost model.max_cost in
	 let cur_term = ref d in
	 let ci = cost_index degree rank in
	   model.sums.(ci) <- model.sums.(ci) -. cost;
	   model.sums.(ci + 1) <- model.sums.(ci + 1) -. 1.0;
	   for t = 2 to degree + 1 do
	     let i = ci + t in
	       model.sums.(i) <- model.sums.(i) -. !cur_term;
	       cur_term := !cur_term *. d;
	   done
       end)


let compute_edge_cost model =
  (** [compute_edge_cost model] make a function that computes the cost
      of an edge using the polynomial in depth.  This does not take
      into account the polynomial in cost which is handled
      separately. *)
  let degree = model.degree in
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
    (fun depth ->
       let d = (float depth) /. max_depthf in
       let coeffs = model.get_coeffs () in
	 (* It may be more efficient to compute [d], and [coeffs] just
	    once per-depth in some cases so lets split this into two
	    functions. *)
	 (fun rank ->
	    if rank = 0 then 0.
	    else begin
	      let ci = cost_index degree rank in
	      let sum = ref coeffs.(ci + 1) in
	      let cur_term = ref d in
		for t = 2 to degree + 1 do
		  let i = ci + t in
		    sum := !sum +. (coeffs.(i) *. !cur_term);
		    cur_term := !cur_term *. d;
		done;
		!sum
	    end))


let get_cost model depth rank cost =
  (** [get_cost model depth rank cost] gets the cost of the edge
      transition, but also takes into account the cost of the root node if
      we are transitioning from depth=0. *)
  let max_children = model.tree.Blfs_tree_stats.max_children in
  let edge_cost =
    model.costs.(max_children * depth + rank)
    +. ((Math.div0 cost model.max_cost) *. model.cost_coeffs.(rank))
  in
    if depth = 0
    then (root_cost model) +. edge_cost
    else edge_cost


let rec n_free model depth =
  let max_children = model.tree.Blfs_tree_stats.max_children in
    if depth = model.tree.Blfs_tree_stats.max_depth
    then 1
    else begin
      let free_branches = ref 1 in
	for r = 1 to max_children - 1 do
	  if (model.costs.(max_children * depth + r) <= 0.
	      && model.cost_coeffs.(r) <= 0.)
	  then incr free_branches
	done;
	let below = n_free model (depth + 1) in
	  1 + below * !free_branches
    end


let  fill_cost_table model =
  (** [fill_cost_table model] fills in the cost table for the model.
      These cost exclude the accumulated cost term.  Costs in this
      table will be monotonically increasing in rank and the left-most
      child will be free.  This also fills in the cost coefficient
      table. *)
  let costs = model.costs in
  let cost_coeffs = model.cost_coeffs in
  let max_depth = model.tree.Blfs_tree_stats.max_depth
  and max_children = model.tree.Blfs_tree_stats.max_children in
    for depth = 0 to max_depth - 1 do
      let edge_cost rank = compute_edge_cost model depth rank in
      let base = depth * max_children in
	for rank = 0 to max_children - 1 do
	  costs.(base + rank) <- edge_cost rank;
	done
    done;
    let coeffs = model.get_coeffs () in
    let degree = model.degree in
      for r = 1 to max_children - 1 do
	cost_coeffs.(r) <- coeffs.(cost_index degree r);
      done


let compute_max_cost model =
  (** [compute_max_cost model] computes the maximum cost of the
      model. *)
  let max_children = model.tree.Blfs_tree_stats.max_children in
  let max_depth = model.tree.Blfs_tree_stats.max_depth in
  let r = max_children - 1 in
  let costs = model.costs in
  let sum = ref 0. in
    for d = 0 to max_depth - 1 do
      sum := !sum +. costs.(d * max_children + r);
    done;
    let m =
      (* This isn't exactly right, now is it?  We need to take the
	 steps in depth into account with the cost coefficient... *)
      !sum +. (Array.fold_left Math.fmax neg_infinity model.cost_coeffs)
    in
      Verb.pr Verb.toplvl "max_cost=%f\n" m;
      model.max_cost <- m


let prepare_for_search model =
  (** [prepare_for_search model] prepares the model for another
      iteration of search. *)
  (* assume fill_cost_table was called before estimation. *)
  (* assumes max_cost was pre-computed. *)
  model.output_average_deviation ();
  Wrarray.fill_all model.sums 0.;
  Blfs_tree_stats.clear model.tree;
  model.sums.((Array.length model.sums) - 1) <- 1.0 (* The constant term. *)


let make_initial_data model =
  (** [make_initial_data model] makes a function that gets the root
      node for the beginning of an iteration of BLFS search. *)
  (fun _ ->
     prepare_for_search model;
     0., 0)


let make_find_bound model =
  (** [make_find_bound model] makes a function that finds the bound
      that should give the desired number of node expansions. *)
  let costs = model.costs in
  let cost_coeffs = model.cost_coeffs in
  let max_children = model.tree.Blfs_tree_stats.max_children in
  let max_bins = model.max_bins in

  let get_child_costs parent_costs depth bound =
    let max_cost = model.max_cost in
    let min_parent_g = Hist.min_val parent_costs in
    let c_hists = ref [] in
      for r = 0 to model.tree.Blfs_tree_stats.max_children - 1 do
	let c = costs.(depth * max_children + r) in
	let p = Blfs_tree_stats.p_with_child_rank model.tree depth r in
	  if min_parent_g +. c <= bound && p > 0.
	  then begin
	    let c0 = Hist.make max_bins in
	    let c1 = Hist.copy parent_costs in
	    let cost_coeff = Math.div0 cost_coeffs.(r) max_cost in
	      Hist.add_mass c p c0;
	      if r > 0 && cost_coeff > 0.
	      then begin
		Hist.multiply c1 cost_coeff;
		let c = Hist.convolve_pruning c0 c1 bound in
		  Hist.normalize p c;
		  Wrutils.push c c_hists
	      end else Wrutils.push c0 c_hists
	  end
      done;
      (* !c_hists is non-empty because the left-most child is free. *)
      Hist.add !c_hists
  in

    (fun desired ->
       fill_cost_table model;
       compute_max_cost model;
       Verb.force Verb.optional
	 (lazy (Wrutils.pr "%d free nodes\n" (n_free model 0)));
       Blfs_bound.find_bound
	 ~max_bins:model.max_bins
	 (Blfs_tree_stats.branch_prob model.tree)
	 get_child_costs
	 ~root_cost:(root_cost model)
	 ~desired)


let search
    leaf_cost
    degree
    max_depth
    max_children
    ?(dump_model=false)
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
  (** [search leaf_cost degree max_depth max_children ?learner_name
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
  let model = make degree learner max_bins max_children max_depth in
  let initial_data = make_initial_data model
  and see_branch = make_see_branch info model
  and see_terminal = make_see_terminal model
  and see_leaf = make_see_leaf leaf_cost model
  and remember_edge = make_remember_edge model
  and forget_edge = make_forget_edge model
  and best_completion = (fun _ _ -> 0.)
  and find_bound = make_find_bound model in
  let iter_children =
    Blfs_regression.make_iter_children info see_branch (get_cost model)
      best_completion remember_edge forget_edge
  in
  let blfs_interface =
    Blfs.make_interface
      ~dump:(if dump_model then (plot_model model) else (fun () -> ()))
      initial_data see_leaf see_terminal iter_children find_bound
  in
    prepare_for_search model;
    Blfs_regression.random_probes info max_children see_branch
      see_leaf see_terminal remember_edge forget_edge initial num_probes;
    Blfs.best_leaf_first_search blfs_interface info 0. initial

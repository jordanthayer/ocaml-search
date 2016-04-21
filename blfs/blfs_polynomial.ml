(** A model that fits a polynomial (in depth) to each child rank in
    order to estimate child cost.

    @author eaburns
    @since 2010-01-08
*)

type t = {
  degree : int;
  (* The degree of the polynomial. *)

  costs : float array;
  (* An array of the pre-computed cost values.  Computed before each
     iteration. *)

  sums : float array;
  (* Sum of the terms for each rank *)

  mutable root_cost : float;
  (* The cost value at the root. *)

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


let const_index degree rank = rank * (degree + 1)
  (** [const_index degree rank] gets the index of the constant term for
      the given rank. *)


let make degree learner max_bins max_children max_depth =
  (** [make degree learner max_bins max_children max_depth]
      makes a new quadratic model. *)
  let n = max_children * (degree + 1) + 1 in
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
      sums = sums;
      root_cost = 0.;
      tree = Blfs_tree_stats.make ~max_children ~max_depth;
      accumulate_deviation = accum_dev;
      output_average_deviation = (fun () -> ignore (output_dev ()));
      max_bins = max_bins;
      show_leaf_cost = show_leaf_cost;
      get_coeffs = get_coeffs;
    }


let plot_model model () =
  (** [plot_model model ()] plots the polynomials. *)
  Printf.printf "----------Plotting model\n%!";
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
      (Wrutils.str "polynomial deg=%d const=%f" degree model.root_cost)
      max_depth !fs


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
  (fun node (_, depth) ->
     Blfs_tree_stats.see_terminal model.tree depth;
     let err = abs_float (model.show_leaf_cost (leaf_cost node)) in
       model.accumulate_deviation err)


let make_remember_edge model =
  (** [make_remember_edge model] makes a function that remembers the
      fact that the search has taken the edge (depth, rank). *)
  let degree = model.degree in
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
    (fun depth rank _ ->
       let d = (float depth) /. max_depthf in
       let cur_term = ref d in
       let ci = const_index degree rank in
	 model.sums.(ci) <- model.sums.(ci) +. 1.0;
	 for t = 1 to degree do
	   let i = ci + t in
	     model.sums.(i) <- model.sums.(i) +. !cur_term;
	     cur_term := !cur_term *. d;
	 done)


let make_forget_edge model =
  (** [make_forget_edge model] makes a function that forgets the fact
      that the search has taken the edge (depth, rank). *)
  let degree = model.degree in
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
    (fun depth rank _ ->
       let d = (float depth) /. max_depthf in
       let cur_term = ref d in
       let ci = const_index degree rank in
	 model.sums.(ci) <- model.sums.(ci) -. 1.0;
	 for t = 1 to degree do
	   let i = ci + t in
	     model.sums.(i) <- model.sums.(i) -. !cur_term;
	     cur_term := !cur_term *. d;
	 done)


let make_compute_edge_cost model =
  (** [make_compute_edge_cost model] make a function that computes the
      cost of an edge. *)
  let degree = model.degree in
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
    (fun depth ->
       let d = (float depth) /. max_depthf in
       let coeffs = model.get_coeffs () in
	 (* It may be more efficient to compute [d], and [coeffs] just
	    once per-depth in some cases so lets split this into two
	    functions. *)
	 (fun rank ->
	    let ci = const_index degree rank in
	    let cur_term = ref d in
	    let sum = ref coeffs.(ci) in
	      for t = 1 to degree do
		let i = ci + t in
		  sum := !sum +. (coeffs.(i) *. !cur_term);
		  cur_term := !cur_term *. d;
	      done;
	      !sum))


let lookup_edge_cost model ~depth ~rank =
  (** [lookup_edge_cost model ~depth ~rank] looks up the edge cost in
      the table. *)
  model.costs.(model.tree.Blfs_tree_stats.max_children * depth + rank)


let get_cost model depth rank _ =
  (** [get_cost model depth rank cost] gets the cost of the edge
      transition, but also takes into account the cost of the root
      node if we are transitioning from depth=0. *)
  let init_c = if depth = 0 then model.root_cost else 0.
  in init_c +. (lookup_edge_cost model ~depth ~rank)


let  fill_cost_table model =
  (** [fill_cost_table model] fills in the cost table for the model.
      Makes each 0th ranked child free but costs are not guaranteed to be
      monotonically increasing in rank. *)
  let costs = model.costs in
  let max_depth = model.tree.Blfs_tree_stats.max_depth
  and max_children = model.tree.Blfs_tree_stats.max_children in
    for depth = 0 to max_depth - 1 do
      let edge_cost = make_compute_edge_cost model depth in
      let base = depth * max_children in
	for rank = 0 to max_children - 1 do
	  costs.(base + rank) <- edge_cost rank;
	done
    done;
    Blfs_regression.clamp_monotonic costs max_depth max_children;
    let rc =
      Blfs_regression.normalize_for_zero_completions
	costs max_depth max_children
    in
    let coeffs = model.get_coeffs () in
      model.root_cost <- rc +. coeffs.((Array.length coeffs) - 1)


let prepare_for_search model =
  (** [prepare_for_search model] prepares the model for another
      iteration of search. *)
  (* assume fill_cost_table was called before estimation. *)
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


let rec n_free model depth =
  let max_depth = model.tree.Blfs_tree_stats.max_depth
  and max_children = model.tree.Blfs_tree_stats.max_children in
    if depth = max_depth
    then 1
    else begin
      let n_zero_branches = ref 0 in
	for r = 0 to max_children - 1 do
	  if lookup_edge_cost model ~depth ~rank:r <= 0.
	  then incr n_zero_branches
	done;
	let below =  n_free model (depth + 1) in
	  1 + below * !n_zero_branches
    end


let make_find_bound model =
  (** [make_find_bound model] makes a function that finds the bound
      that should give the desired number of node expansions. *)
  let max_children = model.tree.Blfs_tree_stats.max_children in
  let get_child_costs parent_costs depth bound =
    let min_parent_g = Hist.min_val parent_costs in
    let c_hist = Hist.make model.max_bins in
      for r = 0 to max_children - 1 do
	let c = lookup_edge_cost model ~depth ~rank:r in
	let p = Blfs_tree_stats.p_with_child_rank model.tree depth r in
	  if min_parent_g +. c <= bound && p > 0. then
	    Hist.add_mass c p c_hist;
      done;
      c_hist
  in
    (fun desired ->
       fill_cost_table model;
(*
       let n_zero = n_free model 0 in
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
    degree
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
(*
  let _, init_coeffs =
    Blfs_regression.make_initializer info degree max_depth max_children
      leaf_cost see_terminal see_branch initial
  in
  let coeffs = model.get_coeffs () in
    Wrarray.copy_into init_coeffs coeffs;
*)
    prepare_for_search model;
    Blfs_regression.random_probes info max_children see_branch
      see_leaf see_terminal remember_edge forget_edge initial num_probes;
    Blfs.best_leaf_first_search blfs_interface info 0. initial

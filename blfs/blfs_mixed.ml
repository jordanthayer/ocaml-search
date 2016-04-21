(** A mixed separate cost and polynomial model.  Begins with the
    polynomial model and switches to the separate model once the separate
    model gives better estimates.

    @author eaburns
    @since 2010-01-12
*)

type t = {
  costs : float array;
  (* An array of the pre-computed cost values.  Computed before each
     iteration. *)

  pattern : float array;
  (* The separate model. *)

  mutable root_cost : float;
  (* The cost at the root node. *)

  degree : int;
  (* The degree of the polynomial. *)

  sums : float array;
  (* The polynomial model. *)

  tree : Blfs_tree_stats.t;
  (* Various statistics on the tree structure: terms, non-terms,
     branching. *)

  mutable use_sep : bool;
  mutable iter_no : int;

  consider_leaf : (float array * float array * float) -> unit;
  (* Considers adding a leaf to the sample. *)

  get_sample : unit -> (float array * float array * float) array;
  (* Gets the uniform sample of instances to train on. *)

  reset_sample : unit -> unit;
  (* Resets the sample selector. *)

  get_reset_sep_dev : unit -> float;
  get_reset_poly_dev : unit -> float;
  (* Tracking average deviation in the learning. *)

  max_bins : int;
  (* Maximum number of histogram bins to use. *)

  show_sep_leaf_cost : float array -> float -> unit;
  get_sep_coeffs : unit -> float array;
  show_poly_leaf_cost : float array -> float -> unit;
  get_poly_coeffs : unit -> float array;
}

let copy_record (sums, pattern, cost) =
  (** [copy_record r] copies a training example for the sample
      selection. *)
 (Array.copy sums, Array.copy pattern, cost)


let index model ~rank ~depth =
  (** [index model rank depth] gets the index into the separate model. *)
  let max_children = model.tree.Blfs_tree_stats.max_children in
    (depth * max_children) + rank


let const_index degree rank = rank * (degree + 1)
  (** [const_index degree rank] gets the index of the constant term for
      the given rank in the polynomial model. *)


let poly_root_cost model =
  (** [poly_root_cost model] gets the cost of the root node in the
      polynomial model. *)
  let coeffs = model.get_poly_coeffs () in
    coeffs.((Array.length coeffs) - 1)


let make learner ~sample_size ~degree ~max_bins ~max_children ~max_depth =
  (** [make learner ~sample_size ~degree ~max_bins ~max_children
      ~max_depth] makes a new model. *)
  let n_sep = max_children * max_depth in
  let n_poly = max_children * (degree + 1) + 1 in
  let pattern = Array.make n_sep 0. in
  let sums = Array.make n_poly 0. in
  let accum_sep_dev, get_reset_sep_dev =
    Blfs_regression.make_error_tracker "separate"
  and accum_poly_dev, get_reset_poly_dev =
    Blfs_regression.make_error_tracker (Wrutils.str "poly deg %d" degree)
  in
  let show_sep_ex, _, get_sep = learner n_sep in
  let show_sep_leaf pattern cost =
    let _, sep_err = show_sep_ex pattern cost in
      accum_sep_dev (abs_float sep_err)
  in
  let show_poly_ex, _, get_poly = learner n_poly in
  let show_poly_leaf sums cost =
    let _, poly_err = show_poly_ex sums cost in
      accum_poly_dev (abs_float poly_err);
  in
  let con, get, reset = Sample.make_stream_sampler sample_size in
    {
      costs = Array.make n_sep 0.;
      pattern = pattern;
      root_cost = 0.;
      sums = sums;
      use_sep = false;
      iter_no = 0;
      get_reset_sep_dev = get_reset_sep_dev;
      get_reset_poly_dev = get_reset_poly_dev;
      degree = degree;
      tree = Blfs_tree_stats.make ~max_children ~max_depth;
      max_bins = max_bins;
      consider_leaf = con ~copy:copy_record;
      get_sample = get;
      reset_sample = reset;
      show_sep_leaf_cost = show_sep_leaf;
      get_sep_coeffs = get_sep;
      show_poly_leaf_cost = show_poly_leaf;
      get_poly_coeffs = get_poly;
    }


let plot_model model () =
  (** [plot_model model ()] makes a plot of the separate model. *)
(*
  let costs = model.get_sep_coeffs () in
*)
  let max_depth = model.tree.Blfs_tree_stats.max_depth in
(*
  let max_children = model.tree.Blfs_tree_stats.max_children in
    Blfs_plot_model.plot_separate
      ~init_depth:1 max_depth max_children costs "separate";
*)
    let degree = model.degree in
    let coeffs = model.get_poly_coeffs () in
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
	(Wrutils.str "polynomial deg=%d const=%f"
	   degree (poly_root_cost model))
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
  let consider_leaf = model.consider_leaf in
    (fun node (cost, depth) ->
       Blfs_tree_stats.see_terminal model.tree depth;
       consider_leaf (model.sums, model.pattern, leaf_cost node))


let train_learners model =
  (** [train_learners model] trains the learner on the current sample
      of leaves and resets the sample. *)
  let show_sep = model.show_sep_leaf_cost in
  let show_poly = model.show_poly_leaf_cost in
  let sample = model.get_sample () in
  let n = Array.length sample in
    Wrarray.permute sample;
    for i = 0 to n - 1 do
      let sums, pattern, cost = sample.(i) in
	show_sep pattern cost;
	if not model.use_sep then show_poly sums cost;
    done;
    model.reset_sample ()


let edge_cost model ~rank ~depth =
  (** [edge_cost model rank depth] computes the cost of an
      edge. *)
  let init_c = if depth = 0 then model.root_cost else 0.
  in init_c +. model.costs.(index model ~rank ~depth)


let make_remember_edge model =
  (** [make_remember_edge model] makes a function that remembers the
      fact that the search has taken the edge (depth, rank). *)
  let degree = model.degree in
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
    (fun depth rank _ ->
       model.pattern.(index model ~rank ~depth) <- 1.0;
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
       model.pattern.(index model ~rank ~depth) <- 0.0;
       let d = (float depth) /. max_depthf in
       let cur_term = ref d in
       let ci = const_index degree rank in
	 model.sums.(ci) <- model.sums.(ci) -. 1.0;
	 for t = 1 to degree do
	   let i = ci + t in
	     model.sums.(i) <- model.sums.(i) -. !cur_term;
	     cur_term := !cur_term *. d;
	 done)


let compute_poly_cost model =
  (** [compute_poly_cost model] make a function that computes the cost
      of an edge using the polynomial. *)
  let degree = model.degree in
  let max_depthf = float model.tree.Blfs_tree_stats.max_depth in
    (fun depth ->
       let d = (float depth) /. max_depthf in
       let coeffs = model.get_poly_coeffs () in
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


let poly_root_cost model =
  (** [poly_root_cost model] gets the cost of the root node in the
      polynomial model. *)
  let coeffs = model.get_poly_coeffs () in
    coeffs.((Array.length coeffs) - 1)


let  poly_fill_costs model costs =
  (** [poly_fill_costs model costs] fills in the cost table using the
      polynomial model.  Makes each 0th ranked child free but costs
      are not guaranteed to be monotonically increasing in rank. *)
  let max_depth = model.tree.Blfs_tree_stats.max_depth
  and max_children = model.tree.Blfs_tree_stats.max_children in
  let root_cost = poly_root_cost model in
    for depth = 0 to max_depth - 1 do
      let edge_cost = compute_poly_cost model depth in
      let base = depth * max_children in
	for rank = 0 to max_children - 1 do
	  costs.(base + rank) <- edge_cost rank;
	  if costs.(base + rank) < 0.
	  then costs.(base + rank) <- 0.;
	  if depth = 0
	  then costs.(base + rank) <- costs.(base + rank) +. root_cost;
	done
    done


let prepare_for_search model =
  (** [prepare_for_search model] prepares the model for another
      iteration of search. *)
  (* prepare model.costs before finding the bound (which happens
     before this). *)
  Wrarray.fill_all model.pattern 0.;
  Wrarray.fill_all model.sums 0.;
  Blfs_tree_stats.clear model.tree


let make_initial_data model =
  (** [make_initial_data model] makes a function that gets the root
      node for the beginning of an iteration of BLFS search. *)
  (fun _ ->
     prepare_for_search model;
     0., 0)

let clamp_coeffs model =
  let max_depth = model.tree.Blfs_tree_stats.max_depth
  and max_children = model.tree.Blfs_tree_stats.max_children in
  let sep_coeffs = model.get_sep_coeffs () in
    Blfs_regression.clamp_monotonic
      sep_coeffs max_depth max_children;
    let rc =
      Blfs_regression.normalize_for_zero_completions
	sep_coeffs max_depth max_children;
    in
      model.root_cost <- rc;
      Wrarray.copy_into sep_coeffs model.costs

let fill_cost_table model =
  (** [fill_cost_table model] decides which model to use (polynomial
      or separate) and then fills in the cost table using the appropriate
      model.  If it switches to the separate then it will never switch
      back. *)
  if not model.use_sep
  then begin
    let poly_dev = model.get_reset_poly_dev ()
    and sep_dev = model.get_reset_sep_dev () in
      if sep_dev < poly_dev
      then begin
	Datafile.write_pairs stdout ["switch to separate",
				     string_of_int model.iter_no ];
	model.use_sep <- true;
      end
  end;
  if model.use_sep
  then begin
    clamp_coeffs model;
    ignore (model.get_reset_sep_dev ());
  end else begin
    poly_fill_costs model model.costs;
    clamp_coeffs model;
  end


let make_find_bound model desired =
  (** [make_find_bound model desired] makes a function that finds the bound
      that should give the desired number of node expansions. *)
  let max_children = model.tree.Blfs_tree_stats.max_children in
  let get_child_costs parent_costs depth bound =
    let min_parent_g = Hist.min_val parent_costs in
    let c_hist = Hist.make model.max_bins in
    let base = depth * max_children in
    let costs = model.costs in
      for r = 0 to max_children - 1 do
	let c = costs.(base + r) in
	let p = Blfs_tree_stats.p_with_child_rank model.tree ~depth ~rank:r in
	  if min_parent_g +. c <= bound then Hist.add_mass c p c_hist;
      done;
      c_hist
  in
    train_learners model;
    fill_cost_table model;
    model.iter_no <- model.iter_no + 1;
    Blfs_bound.find_bound
      ~max_bins:model.max_bins
      (Blfs_tree_stats.branch_prob model.tree)
      get_child_costs
      ~root_cost:model.root_cost
      ~desired


let search
    leaf_cost
    degree
    max_depth
    max_children
    ?(sample_size = 4096)
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
  let model =
    make learner ~sample_size ~degree ~max_bins ~max_children ~max_depth
  in
  let initial_data = make_initial_data model
  and see_branch = make_see_branch info model
  and see_terminal = make_see_terminal model
  and see_leaf = make_see_leaf leaf_cost model
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
(*
  let f, init_coeffs =
    Blfs_regression.make_initializer info degree max_depth max_children
      leaf_cost see_terminal see_branch initial
  in
  let poly_coeffs = model.get_poly_coeffs () in
  let sep_coeffs = model.get_sep_coeffs () in
    Wrarray.copy_into init_coeffs poly_coeffs;
    for r = 0 to max_children - 1 do
      for d = 0 to max_depth - 1 do
	sep_coeffs.(index model ~rank:r ~depth:d) <- Math.fmax (f d r) 0.
      done;
    done;
*)
    prepare_for_search model;
    Blfs_regression.random_probes info max_children see_branch
      see_leaf see_terminal remember_edge forget_edge initial num_probes;
    Blfs.best_leaf_first_search blfs_interface info 0. initial

(** Functions for finding the next bound in a BLFS search.

    @author eaburns
    @since 2010-01-08
*)


(**** debugging ***)


let check h =
  Hist.check h


let print_hist h name =
  Printf.printf "--- %s ---\n" name;
  Hist.fprint stdout h


let dump_id = ref 0
and dump_prefix = "/tmp/hist-dump"
and do_dumping = ref false

let dump h s d =
(*
  if !do_dumping then
    (Hist.check h;
     incr dump_id;
     let path = Wrutils.str "%s-%d.ps" dump_prefix !dump_id
     and title = Wrutils.str "%s at %d (%6f total)" s d (Hist.total_weight h) in
       Hist_plot.plot ~title:title path h)
*)
()

(*
let collect_dumped s =
  dump_id := 0;
  let files = List.filter (Wrstr.starts_as dump_prefix)
    (fst (Wrfname.dir_contents (Filename.dirname dump_prefix))) in
    Ps_plot.montage ~across:6 ~down:8 ~spacing:0.1
      files (Wrutils.str "/tilde/ruml/%s.ps" s);
    List.iter Sys.remove files
*)


(**** end debugging ****)


let branch_prob non_term term d (*avg*) =
  (** prob that child of node at level d is NOT a leaf *)
  let t = term.(d)
  and nt = non_term.(d) in
  let total = nt + t in
    if total = 0 then
      (* if at max depth, then 0. else, 1. *)
      if d = (Array.length term - 1) then
        0.
      else
        1. (* avg TODO - should make this an average *)
    else (float nt) /. (float total)


let assured_descendants max_depth non_term term =
  (** returns array in which element d is number of descendant nodes a
      node at depth d will have if we only count the zero-cost child.
      Includes an element max+1 to simplify look-ahead. *)
  (* number of nodes = sum_{i=0}^{D} product_{j=i}^{D} p_j *)
  let a = Array.make (max_depth + 1) 0. in
    for d = max_depth - 1 downto 0 do
      let p = (let total = non_term.(d) + term.(d) in
		 if total = 0 then 0. else Math.div non_term.(d) total) in
	a.(d) <- p +. (p *. a.(d+1))
    done;
    a


let init_hist_with mb value wt =
  let h = Hist.make mb in
    Hist.add_mass value wt h;
    h


let init_hist mb value = init_hist_with mb value 1.0


let norm_to_one h =
  (** destructive *)
  Hist.normalize 1. h


let children_hists max_bins ~get_child_cost get_child_h max_children d
    max_val curr_nodes =
  (** [children_hists get_child_cost get_child_h max_children d
      max_val curr_nodes] gets the child cost and child h histograms
      for children generated at depth [d]. *)
  let min = Hist.min_val curr_nodes in
  let max_child = max_val -. min in
  let child_cost_hists =
    Wrutils.map_n_opt
      (fun i -> get_child_cost d i max_child)
      (max_children - 1) in
  let child_h_hists =
    Wrutils.map_n_opt (fun i -> get_child_h d i) (max_children - 1)
  in
    (try
       assert (child_cost_hists <> []);
     with _ ->
       Wrutils.pr "depth=%d, max_val=%f, min=%f max_child=%f\n"
	 d max_val min max_child;
       assert false);
    List.iter norm_to_one child_cost_hists;
    let child_costs = Hist.add child_cost_hists in
(*
      Wrutils.pr "%f parents, %f children\n" (Hist.total_weight curr_nodes)
	(Hist.total_weight child_costs);
*)
      let child_hs =
	if child_h_hists = []
	then init_hist max_bins 0.
	else begin
	  let h = Hist.add child_h_hists in
	    norm_to_one h;
	    h
	end
      in
(*
	Verb.force 5 (lazy (List.iter check child_cost_hists;
			    print_hist child_costs "Children";
			    check child_costs));
*)
	child_costs, child_hs


let prune_value desired total_fs curr_fs curr_nodes descendants =
  (** [prune_value desired total_fs curr_fs curr_nodes descendants]
      prunes the histograms when [desired] nodes are desired. *)
  let factor = (* max 1. *) descendants in
    (* assuming zero-cost path to leaf for each node in current! *)
  let max_val =
    Hist.bound_for_wted_combo desired total_fs curr_fs factor
  in
    Verb.pr Verb.debug "Given %f descendants, pruning at %f for %.1f.\n"
      factor max_val desired;
    (* if we already have enough nodes, bound can only come down *)
    Hist.prune_value_right max_val total_fs;
    (* assuming that child costs are non-negative *)
    Hist.prune_value_right max_val curr_nodes;
    Hist.prune_value_right max_val curr_fs;
    max_val


let propagate_level max_bins ~get_child_cost get_child_h
    max_children branch_prob descendants convolve desired total_fs
    curr_fs curr_nodes d =
  (** [propagate_level max_bins get_child_cost get_child_h max_children
      branch_prob descendants convolve desired total_fs curr_fs
      curr_nodes d] simulates current nodes going through arcs from
      level [d].  destructive on [total_fs] and [curr_nodes].
      returns false when no more nodes left to propagate.

      [max_bins] is the maximum number of bins to use when creating a
      new histogram.

      [get_child_cost] is a function from depth, rank and max cost to
      a child cost histogram.

      [get_child_h] is a function from depth and rank to a child
      cost-to-go estimate histogram.

      [max_children] is the maximum number of children for any search
      node.

      [branch_prob] is a function from the search depth to the
      probability that there is a branch (as opposed to a leaf or
      prune).

      [descendants] is an array where each element contains the number
      of assured children at each depth.

      [convolve] is the convolution function being used.

      [desired] is the desired number of nodes.

      [total_fs] is a histogram with the estimated solution cost under
      each nodes seen so far.

      [curr_fs] is a histogram of the estimated solution cost under
      each node in the previous layer.  (this is only used for more
      pruning).

      [curr_nodes] is a histogram of the accrued cost of each node at
      the previous layer.

      [d] is the depth to simulate.
  *)
  Verb.force Verb.debug
    (lazy (Printf.printf
	     "==================== Start of depth %d. ====================\n"
	     d;
	   print_hist !total_fs "total_fs (accumed_fs)";
	   print_hist !curr_nodes "curr_nodes (parent_gs)";
	   check !total_fs; check !curr_nodes));
  dump !total_fs "total" d;
  dump !curr_nodes "current" d;
  let branch_p = branch_prob d in
    if branch_p = 0. then
      false
    else
      (* prune for new total *)
      let max_val =
	prune_value desired !total_fs !curr_fs !curr_nodes descendants.(d)
      in
      let child_costs, child_hs =
	children_hists max_bins ~get_child_cost get_child_h max_children
	  d max_val !curr_nodes
      in
	Hist.scale branch_p !curr_nodes;
	curr_nodes := convolve !curr_nodes child_costs max_val;
	curr_fs := convolve !curr_nodes child_hs max_val;
	Verb.force Verb.debug
	  (lazy (
	     print_hist child_costs "child_costs (edge_costs)";
	     print_hist child_hs "child_hs (cost_and_hs)";
	     print_hist !curr_nodes "curr_nodes (child_gs)";
	     print_hist !curr_fs "curr_fs (child_fs)";
		));
	(* prune for new curr (upper bound because total not updated) *)
	ignore (prune_value desired !total_fs !curr_fs !curr_nodes
		  descendants.(d + 1));
	total_fs := Hist.add [!curr_fs; !total_fs];
	true


let find_bound_for_nodes max_depth max_children max_bins root_cost root_f
    ~get_child_cost ?(get_child_h=(fun _ _ -> None))
    non_term term ?(convolve = Hist.convolve_pruning) nodes =
  (** [find_bound_for_nodes max_depth max_children max_bins root_cost root_f
      get_child_cost ?get_child_h non_term term ?convolve nodes]

      destructive on the child histograms returned by
      [get_child_cost].  Assumes that there is always a zero-cost
      child.  This allows combining total and curr costs dists to
      derive a pruning bound for total_fs. *)
  let desired = (float_of_int nodes)
  and descendants = assured_descendants max_depth non_term term
  and branch_prob = branch_prob non_term term
    (* the number of nodes at the current depth at each cost *)
  and curr_nodes = ref (init_hist max_bins root_cost)
    (* the total number of nodes generated so far at each cost, up
       through the current depth.  The root doesn't count as generated.

       The cost distribution of the layer.
    *)
  and curr_fs = ref (init_hist max_bins root_f)
  and total_fs = ref (Hist.make max_bins)
    (* d is depth we are simulating leaving. no arcs leave max_depth *)
  and d = ref 0 in
(*
    Wrutils.pr "root f=%f\n%!" root_f;
    Wrutils.pr "root cost=%f\n%!" root_cost;
*)
    while (propagate_level max_bins ~get_child_cost get_child_h
	     max_children branch_prob descendants convolve desired
	     total_fs curr_fs curr_nodes !d) do
      incr d
    done;
    Verb.pr Verb.optional "Finding bound for %d\n" nodes;
    Verb.force 5 (lazy (print_hist !total_fs "Final"));
    dump !total_fs "final" (-1);
    (Hist.val_for_weight desired !total_fs), nodes

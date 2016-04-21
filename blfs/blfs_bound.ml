(** A re-write of the bound estimation code.

    @author eaburns
    @since 2010-01-11
*)


let print_hist h name =
  Printf.printf "--- %s ---\n" name;
  Hist.fprint stdout h


(** Simulates one depth level of search. *)
let simulate_expand convolve get_child_costs ~depth parents ~branching
    ~bound ~desired =
  let edge_costs = get_child_costs parents depth bound in
    Hist.scale branching edge_costs;
    if Verb.level Verb.debug then print_hist edge_costs "edge_costs";
    let child_costs =
      if Hist.has_mass edge_costs && Hist.has_mass parents then
	convolve parents edge_costs bound
      else
	edge_costs
    in child_costs


(** Simulates a search.  The result is the accumlated f value
    distribution. *)
let rec simulate_search
    convolve br_prob get_child_costs ~depth ~accum ~parents ~bound ~desired =
  let branching = br_prob depth in
    if branching = 0. then
      accum
    else begin
      if Verb.level Verb.debug then begin
	Wrutils.pr "==================== ";
	Wrutils.pr "depth %d " depth;
	Wrutils.pr "====================\n";
	print_hist accum "accum";
	print_hist parents "parents";
      end;

      let child_costs =
	simulate_expand convolve get_child_costs ~depth parents
	  ~branching ~bound ~desired
      in
	if Verb.level Verb.debug then begin
	  Wrutils.pr "bound=%f\n" bound;
	  print_hist child_costs "child_costs";
	end;

	let bound =
	  Hist.bound_for_wted_combo desired accum child_costs 1.0
	in
	  Hist.prune_value_right bound accum;
	  Hist.prune_value_right bound child_costs;
	  let accum = Hist.add [accum; child_costs] in

	    simulate_search convolve br_prob get_child_costs ~depth:(depth + 1)
	      ~accum ~parents:child_costs ~bound ~desired
    end


(** Finds a bound that should give [desired] nodes using the model
    accessed by [get_child_hists].

    [max_bins] defines the size of the histograms that are created.

    [br_prob] is a function from depth to the probability of a
    branch at the given depth.

    [get_child_costs] is a function from depth, parents and
    bound to child *edge* cost histogram.

    [root_cost] is the cost of the root node.

    [desired] is the desired number of node expansions. *)
let find_bound ?(convolve=Hist.convolve_pruning)
    ~max_bins br_prob get_child_costs ~root_cost ~desired =
  Verb.pr Verb.optional "Finding bound for %d\n" desired;
  let desiredf = float desired in
  let accum = Hist.make max_bins
  and parents = Hist.make max_bins in
    Hist.add_mass root_cost 1.0 parents;
    let total_costs = (simulate_search
			 convolve
			 br_prob
			 get_child_costs
			 ~depth:0
			 ~accum
			 ~parents
			 ~bound:infinity
			 ~desired:desiredf)
    in
    let vl = Hist.val_for_weight desiredf total_costs in
    let predicted = truncate (Hist.weight_left_of vl total_costs) in
      vl, predicted

(* $Id: separate.ml,v 1.6 2004/06/14 20:30:53 ruml Exp ruml $

   the separate action cost model

   each child rank at each depth gets a separate action cost.  leaf
   cost is predicted as the sum of the costs along its path.

   the data passed down is the current sum so far and the current depth


   root is at depth 0, next is depth 1.  If max_depth is 2, then there
   are three levels of nodes, two layers of arcs.
*)


type t = {
  non_term : int array;
  (* numbers of non-terminal nodes *expanded* at a given depth. *)

  term : int array;
  (* Number of terminal nodes seen at a given depth. For finding
     average of leaf probability for when we have no data *)

  nchildren_counts : int array array;
  (* Number of children seen for each rank at each depth. *)

  total_counts : int array;
  (* The number of nodes seen at a given depth.  This is computed by
     using the [num_children] function. *)

  max_children : int;
  (* filled in during descent, passed to learner.  no entries for max_depth *)

  pattern : float array;
  (* There is an entry for each (depth x rank).  The pattern has a 1
     for entries where the tree is searching under the node at the
     given depth and rank and a zero elsewhere. *)
  (* SHOULD WE ALSO CACHE DEPTH AND CURR_COST HERE in addition to pattern? *)

  saved : float array;
  (* Static copy for use during search *)

  best_completions : float array;
  (* The best completions for each depth. *)

  show_ex : float -> unit;
  (* Show a training example to the learner. *)

  get_params : unit -> float array;
  (* Current param array is owned by learner, this provides access.
     No entries for max_depth, since no arcs out.  last entry is root
     cost *)

  max_bins : int;
  (* Maximum number of bins to use in the histograms. *)
}


let max_depth m =
  (** deepest depth at which we could encounter a node *)
  (Array.length m.non_term) - 1


let params m =
  (** costs on arcs, so no layer for nodes at max_depth *)
  let ps = m.get_params () in
    (*
      (try
      assert (Wrarray.for_all ((<=) 0.) ps);
      with _ ->
      Array.iteri (fun i vl -> Wrutils.pr "%02d: %f\n" i vl) ps;
      assert false);
    *)
    ps


(************* reiniting model between passes *****************)


let smallest_diff a max_d max_b =
  (** returns the smallest non-zero difference between any adjacent
      child costs, or None *)
  let min = ref infinity in
    for d = 0 to max_d - 1 do
      let start = d * max_b in
      let prev = ref a.(start) in
	for b = 1 to max_b - 1 do
	  let this = a.(start + b) in
	    (* assume monotonically nondecreasing, so diff is nonnegative *)
	  let diff = this -. !prev in
	    if (diff > 0.) && (diff < !min)
	    then min := diff;
	    prev := this
	done
    done;
    if !min = infinity
    then None
    else Some !min


let enforce_diff a max_d max_b diff =
  (** destructive on [a] *)
  for d = 0 to max_d - 1 do
    let start = d * max_b in
    let prev = ref a.(start) in
      for i = start + 1 to start + max_b - 1 do
	let min = !prev +. diff
	and this = a.(i) in
	let value = (if this >= min
		     then this
		     else !prev +. (diff *. (0.5 +. (Random.float 1.))))
	in
	  a.(i) <- value;
	  prev := value
      done
  done


let clamp_monotonic a max_d max_b =
  (** force child costs to be monotonically increasing at each depth *)
  for d = 0 to max_d - 1 do
    Functions.monotonic_regression a (d * max_b) max_b
  done;
  let diff = (match smallest_diff a max_d max_b with
		  Some d -> d
		| None -> (a.(0) /. 20.) /. (float_of_int max_d)) in
    enforce_diff a max_d max_b diff


let normalize_for_zero_completions p max_d max_b =
  (** transform child costs so that after the first arc, the preferred
      child always costs zero.  assumes already monotonic increasing in
      child rank at each depth *)
  let backed_up = ref 0. in
    for d = max_d - 1 downto 1 do
      let base = d * max_b in
      let best = p.(base) in
	p.(base) <- 0.;
	for i = base + 1 to base + max_b - 1 do
	  p.(i) <- p.(i) -. best
	done;
	backed_up := !backed_up +. best
    done;
    for i = 0 to max_b - 1 do
      p.(i) <- p.(i) +. !backed_up
    done


let adjust_for_new_estimates m =
  (** clean-up between passes, before estimating new bound *)
  let a = params m
  and max_d = max_depth m
  and max_b = m.max_children in
    clamp_monotonic a max_d max_b;
    normalize_for_zero_completions a max_d max_b


let write_params label vec =
  Wrutils.pr label;
  Vector.write_line stdout vec



let best_cost m d =
  (** [best_cost m d] best cost gets the best cost at a given
      depth. *)
  let max_children = m.max_children in
  let base = d * max_children in
  let costs = m.saved in
  let minimum = ref infinity in
    for c = 0 to max_children - 1 do
      let cost = costs.(base + c) in
	if cost < !minimum then minimum := cost
    done;
    !minimum


let best_completions m =
  (** [best_completions m] get an array of the best possible
      completions for each depth. *)
  let max_depth = max_depth m in
  let leaf_depth = max_depth - 1 in
  let completions = Array.make (max_depth + 1) 0. in
    for d = leaf_depth - 1 downto 0 do
      let p = Blfs_bound_old.branch_prob m.non_term m.term d in
      let cheapest_action = best_cost m d in
	completions.(d) <- p *. (cheapest_action +. completions.(d + 1));
    done;
    completions


let prepare_for_search m =
  (* write_params "Model for prev pass was: " m.saved;
     write_params "Model for next pass is: " (params m); *)
  Wrarray.copy_into (params m) m.saved;
  Wrarray.fill_all m.non_term 0;
  Wrarray.fill_all m.term 0;
  Wrarray.fill_all m.total_counts 0;
  Array.iter (fun ary -> Wrarray.fill_all ary 0) m.nchildren_counts;
  (* may have garbage if previous iteration was halted *)
  Wrarray.fill_all m.pattern 0.


let initial_data m _ =
  prepare_for_search m;
  0., 0


(************** updating model ************)


let update_term m _ data =
  (** used at prunes and leaves *)
  match data with
      _,depth -> m.term.(depth) <- m.term.(depth) + 1
	(*TODO incr m.leaf_count; incr m.total*)


let make_update_leaf m get_cost =
  (fun node data ->
     let c = get_cost node in
       assert (c >= 0.);
       update_term m node data;
       m.show_ex c)


let see_nchildren m depth nchildren =
  let nchildren_counts = m.nchildren_counts.(depth) in
    for n = 0 to nchildren - 1 do
      nchildren_counts.(n) <- nchildren_counts.(n) + 1;
    done;
    m.total_counts.(depth) <- m.total_counts.(depth) + 1


let p_with_child_rank m parent_depth rank =
  (** [p_with_child_rank m parent_depth rank] gets the proportion of
      parents at depth [parent_depth] with a child of rank [rank]. *)
  let total = m.total_counts.(parent_depth) in
    if total = 0
    then 1.0				(* anything is possible. *)
    else (float m.nchildren_counts.(parent_depth).(rank)) /. (float total)



(************* initialization before blfs *****************)


exception Halted


let rec get_random_child info node nchildren =
  let i = Random.int nchildren in
    (Info.get_child info node i), i


let random_probe m get_cost info root =
  (** returns true iff reached a leaf *)
  let rec visit n depth =
    if (Info.halt_p info)
    then raise Halted
    else if (Info.leaf_p info n)
    then (m.term.(depth) <- m.term.(depth) + 1; (* TODO - add average *)
	  m.show_ex (get_cost n);
	  if (Info.check_best_and_optimal info n)
	  then raise Halted;
	  true)
    else (Info.incr_branches info;
	  m.non_term.(depth) <- m.non_term.(depth) + 1;
	  (* TODO - add average *)
	  let nchildren = Info.num_children info n in
	  let child, index = get_random_child info n nchildren in
	  let i = (depth * m.max_children) + index in
	    see_nchildren m depth nchildren;
	    m.pattern.(i) <- 1.;
	    let res = visit child (depth + 1) in
	      m.pattern.(i) <- 0.;
	      res)
  in
    visit root 0


let always_take_child m get_cost info root child =
  (** Always takes the [child]'th child.  If [child] > (max_children
      n) then the max child is taken.  *)
  let rec visit n depth =
    if (Info.halt_p info)
    then raise Halted
    else if (Info.leaf_p info n)
    then (m.term.(depth) <- m.term.(depth) + 1; (* TODO - add average *)
	  m.show_ex (get_cost n);
	  if (Info.check_best_and_optimal info n)
	  then raise Halted;
	  true)
    else (Info.incr_branches info;
	  m.non_term.(depth) <- m.non_term.(depth) + 1;
	  (* TODO - add average *)
	  let nchildren = Info.num_children info n in
	  let index = min (nchildren - 1) child in
	  let child = Info.get_child info n index in
	  let i = (depth * m.max_children) + index in
	    see_nchildren m depth nchildren;
	    m.pattern.(i) <- 1.;
	    let res = visit child (depth + 1) in
	      m.pattern.(i) <- 0.;
	      res)
  in
    visit root 0


let random_probes model get_cost info initial num_probes =
  try
    let successes = ref 0 in
    let max_ch = model.max_children in
      ignore (always_take_child model get_cost info initial 0);
      ignore (always_take_child model get_cost info initial max_ch);
      while !successes < num_probes do
	if (random_probe model get_cost info initial)
	then incr successes
      done;
  with Halted -> ()


(************** using model to choose children ************)


let map_children num_children m =
  let max_c = m.max_children
  and pat = m.pattern
  and costs = m.saved
  and best_completion = m.best_completions in
    (fun do_child notify_skipping n (so_far, depth) bound ->
       (** controls which children blfs will take.  assumes that costs
	   have been adjusted so that completion cost is zero, and so
	   costs are non-decreasing with child rank. *)
       m.non_term.(depth) <- m.non_term.(depth) + 1;
       let h = best_completion.(depth + 1) in
       let base = depth * max_c
       and next_depth = depth + 1 in
	 (* always visit the most preferred child! *)
	 pat.(base) <- 1.;
	 do_child n 0 (so_far +. costs.(base), next_depth);
	 pat.(base) <- 0.;
	 let i = ref (base + 1) in
	 let nchildren = num_children n in
	 let max_i = base + nchildren
	 and skip = ref false in
	   see_nchildren m (depth + 1) nchildren;
	   (* now (possibly) search children with rank > 0. *)
	   while (not !skip) && (!i < max_i) do
	     let g = so_far +. costs.(!i) in
	     let f = g +. h in
	       if f <= bound then
		 (pat.(!i) <- 1.;
		  do_child n (!i - base) (g, next_depth);
		  pat.(!i) <- 0.)
	       else
		 (notify_skipping ();
		  skip := true);
	       i := !i + 1;
	   done)


(************** estimating the bound ************)

(* bound_for_nodes0 uses the old bound estimation code. *)
let bound_for_nodes0 m nodes =
  (** return threshold for next pass *)
  if (nodes < 1) then (write_params "saved params" m.saved;
		       write_params "current params" (params m);
		       failwith "negative number of nodes desired!");
  adjust_for_new_estimates m;
  Wrarray.copy_into (best_completions m) m.best_completions;
  let costs = params m in
  let get_child_cost depth rank max_cost =
    let start = depth * m.max_children in
    let g = costs.(start + rank) in
      if g <= max_cost
      then begin
	let p = p_with_child_rank m depth rank in
	  Some (Blfs_bound_old.init_hist_with m.max_bins g p)
      end else None
  in
  let get_child_h depth rank =
    let p = p_with_child_rank m depth rank in
      Some (Blfs_bound_old.init_hist_with m.max_bins
	      m.best_completions.(depth + 1) p)
  in
    Blfs_bound_old.find_bound_for_nodes
      (max_depth m) m.max_children m.max_bins 0. (m.best_completions.(0))
      ~get_child_cost ~get_child_h m.non_term m.term nodes



(* bound_for_nodes2 uses the new bound estimation code. *)
let bound_for_nodes2 m nodes =
  (** return threshold for next pass *)
  if (nodes < 1) then (write_params "saved params" m.saved;
		       write_params "current params" (params m);
		       failwith "negative number of nodes desired!");
  adjust_for_new_estimates m;
  Wrarray.copy_into (best_completions m) m.best_completions;
  let costs = params m in
  let get_child_costs parent_costs depth bound =
    let min_parent_g = Hist.min_val parent_costs in
    let c_hist = Hist.make m.max_bins in
    let start = depth * m.max_children in
      for r = 0 to m.max_children - 1 do
	let c = costs.(start + r) in
	let p = p_with_child_rank m depth r in
	  if min_parent_g +. c < bound then Hist.add_mass c p c_hist;
      done;
      c_hist
  in
    Blfs_bound.find_bound
      ~max_bins:m.max_bins
      (Blfs_bound_old.branch_prob m.non_term m.term)
      get_child_costs
      ~root_cost:0.
      ~desired:nodes

let bound_for_nodes = bound_for_nodes2

(********** packaging up the interface *************)

let plot_model model () =
  (** [plot_model model ()] makes a plot of the separate model. *)
  let costs = params model in
  let max_depth = max_depth model
  and max_children = model.max_children in
  let ranks = Array.make_matrix max_children max_depth (0., 0.) in
    for r = 0 to max_children - 1 do
      let depths = ranks.(r) in
	for d = 0 to max_depth - 1 do
	  let ind = d * max_children + r in
	    depths.(d) <- (float d), costs.(ind);
	done
    done;
    Blfs_plot_model.plot_points "separate" (Array.to_list ranks)


let make_model max_depth max_children max_bins learner =
  let n = max_depth * max_children in
  let pat = Array.make n 0. in
  let show, _, get = learner n
  in
    { non_term = Array.make (max_depth + 1) 0;
      term = Array.make (max_depth + 1) 0;
      nchildren_counts =
	Array.make_matrix (max_depth + 1) max_children 0;
      total_counts = Array.make (max_depth + 1) 0;
      max_children = max_children;
      pattern = pat;
      saved = Array.make n 0.;
      show_ex = (fun cost -> ignore (show pat cost));
      get_params = get;
      max_bins = max_bins;
      best_completions = Array.make (max_depth + 1) 0.;
    }


let make_blfs_interface dump_model num_children max_depth
    max_children max_bins learner get_leaf_cost =
  let m = make_model max_depth max_children max_bins learner in
  let i = Blfs.make_interface
    ~dump:(if dump_model then (plot_model m) else (fun () -> ()))
    (initial_data m)
    (make_update_leaf m get_leaf_cost)
    (update_term m)
    (map_children num_children m)
    (bound_for_nodes m)
  in i, m


(************* integration with BLFS *****************)


let lms rate n =
  (** avoid problems with inferring type of [learner] argument to
      [blfs_separate] *)
  Lms.init_lms ~learning_rate:rate n

let nlmsm rate n =
  Lms.init_nlms_momentum ~learning_rate:rate n


let nlms_flow_2 rate n =
  Lms.init_nlms_flow ~initial_learning_rate:rate n


let learner_by_name name =
  match name with
    | "lms" -> lms
    | "nlmsm" -> nlmsm
    | "nlms_flow_2" -> nlms_flow_2
    | _ ->
	invalid_arg
	  (name ^ ": is not a learner (try: lms, nlmsm or nlms_flow_2")


let blfs_separate max_depth max_children get_leaf_cost
    ?(dump_model=false)
    ?(learner_name  = "lms")
    ?(learning_rate = 0.000001)
    ?(max_bins = 200)
    ?(num_probes = max_depth / 2)
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
    (** nodes at [max_depth] will never have children (root is at depth
	0). *)
    =
  let learner = learner_by_name learner_name learning_rate in
  let info = Info.make num_children get_child leaf_p better_p optimal_p
    copy_state prune_p log prev_best halt in
  let interface, model =
    make_blfs_interface dump_model (Info.num_children info)
      max_depth max_children max_bins learner get_leaf_cost
  in
    if (Info.optimal_so_far info)
    then ((Info.curr_best info), (Info.stats info), true,
	  (Info.leaf_or_prune_p info initial))
    else
      (random_probes model get_leaf_cost info initial num_probes;
       Blfs.best_leaf_first_search interface info 0. initial)


(* EOF *)

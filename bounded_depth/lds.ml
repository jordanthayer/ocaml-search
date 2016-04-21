(* $Id: lds.ml,v 1.1 2003/07/17 19:12:42 ruml Exp ruml $

   lds and ilds
*)


exception Halted


(******** limited-discrepancy search

  can easily support an indexed scheme in which child n costs n discrepancies

  don't need meaningful depth bound
********)


(******** `improved' limited-discrepancy search ********)

(* counts all not-most-preferred children as one discrepancy *)


let ilds_iteration node info max_depth num_discreps brancher =
  (** main ilds function, parameterized by the branching policy *)
  let rec visit node discreps_needed depth_remaining =
    (** returns true if discreps were used up.  if no branch uses up the
      discrepancies, this indicates that we have explored the entire tree.
      side-effects on info *)
    assert (discreps_needed >= 0);
    assert (depth_remaining >= discreps_needed);
    if (Info.halt_p info) then
      raise Halted
    else if (Info.leaf_p info node) then
      (if (Info.optimal_p info node) then
	 raise Halted;
       discreps_needed = 0)
    else if (Info.prune_p info node) || (Info.num_children info node) = 0 then
      discreps_needed = 0
    else
      (Info.incr_branches info;
       brancher node info discreps_needed depth_remaining visit)
  in
    visit node num_discreps max_depth


let visit_non_preferred n info discreps_needed depth_remaining visit =
  if (discreps_needed > 0) then
    let used_up = ref false in
      for i = 1 to (Info.num_children info n) - 1 do
	if (visit (Info.get_child info n i) (discreps_needed - 1)
	      (depth_remaining - 1)) then
	  used_up := true
      done;
      !used_up
  else
    false


let visit_preferred n info discreps_needed depth_remaining visit =
  if (depth_remaining > discreps_needed) then
    visit (Info.get_child info n 0) discreps_needed (depth_remaining - 1)
  else
    false


let ilds_top_brancher n info discreps_needed depth_remaining visit =
  let non = visit_non_preferred n info discreps_needed depth_remaining visit in
  let pref = visit_preferred n info discreps_needed depth_remaining visit in
    non || pref


let ilds_bottom_brancher n info discreps_needed depth_remaining visit =
  let pref = visit_preferred n info discreps_needed depth_remaining visit in
  let non = visit_non_preferred n info discreps_needed depth_remaining visit in
    pref || non


let ilds_iterations ?(start_disc = 0) node info max_depth brancher =
  (** returns true when tree is exhasuted *)
  let rec next_pass num_discreps =
    let used_all = ilds_iteration node info max_depth num_discreps brancher in
      if (num_discreps = max_depth) then
	true
      else if not used_all then
	(* no branch used all discrepancies - done with tree! *)
	true
      else
	(Verb.pe Verb.debug "Current disc:%i\n%!" (num_discreps+1);
	 next_pass (num_discreps + 1))
  in
    next_pass start_disc


type discrepancy_order = TopFirst | BottomFirst


let ilds_top
  (* returns optional node, stats, optimal (does sol satisfy optimal_p),
     complete (was entire space searched) *)
  ?(discrep_order = TopFirst)
  ?(optimal_p = (Fn.constantly1 false))
  ?(prune_p = (Fn.constantly2 false))
  ?(prev_best = None)
  ?(halt = [Info.Never])
  ?(log = Fn.no_op2)
  ?(start_disc = 0)
  copy_state
  max_depth
  better_p
  leaf_p
  num_children
  get_child
  initial
  =
  let info = Info.make num_children get_child leaf_p better_p optimal_p
	       copy_state prune_p log prev_best halt
  and brancher = (match discrep_order with
		    TopFirst -> ilds_top_brancher
		  | BottomFirst -> ilds_bottom_brancher) in
  let complete = (if Info.optimal_so_far info then
		    Info.leaf_or_prune_p info initial
		  else
		    try
		      ilds_iterations ~start_disc initial
			info max_depth brancher
		    with Halted | Sys.Break -> false)
  in
    (Info.curr_best info),
    (Info.stats info),
    (Info.optimal_so_far info),
    complete


let ilds_bottom
  ?(optimal_p = (Fn.constantly1 false))
  ?(prune_p = (Fn.constantly2 false))
  ?(prev_best = None)
  ?(halt = [Info.Never])
  ?(log = Fn.no_op2)
  ?(start_disc = 0)
  copy_state
  max_depth
  better_p
  leaf_p
  get_child
  initial
  = ilds_top
      ~discrep_order:BottomFirst
      ~optimal_p ~prune_p ~prev_best ~halt ~log ~start_disc
      copy_state
      max_depth
      better_p
      leaf_p
      get_child
      initial

(* EOF *)

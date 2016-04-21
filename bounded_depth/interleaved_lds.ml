(**

   @author jtd7
   @since 2010-12-20
   Interleaved limited discrepancy search.
*)

exception Halted

let ilds_iteration node info max_depth num_discreps brancher =
  (** main ilds function, parameterized by the branching policy *)
  let rec visit node discreps_needed depth_remaining =
    (** returns true if discreps were used up.  if no branch uses up the
	discrepancies, this indicates that we have explored the entire tree.
	side-effects on info *)
    assert (discreps_needed >= 0);
    assert (depth_remaining >= discreps_needed);
    if (Info.halt_p info)
    then raise Halted
    else if (Info.leaf_p info node)
    then (if (Info.optimal_p info node)
	  then raise Halted;
	  discreps_needed = 0)
    else if (Info.prune_p info node) || ((Info.num_children info node) = 0)
    then (discreps_needed = 0)
    else (Info.incr_branches info;
	  brancher node info discreps_needed depth_remaining visit)
  in
    visit node num_discreps max_depth


let visit_non_preferred n info discreps_needed depth_remaining visit =
  if (discreps_needed > 0)
  then (let used_up = ref false in
	  for i = 1 to (Info.num_children info n) - 1 do
	    if (visit (Info.get_child info n i) (discreps_needed - 1)
		  (depth_remaining - 1))
	    then used_up := true
	  done;
	  !used_up)
  else false


let visit_preferred n info discreps_needed depth_remaining visit =
  if (depth_remaining > discreps_needed)
  then visit (Info.get_child info n 0) discreps_needed (depth_remaining - 1)
  else false


let ilds_top_brancher n info discreps_needed depth_remaining visit =
  let non = visit_non_preferred n info discreps_needed depth_remaining visit in
  let pref = visit_preferred n info discreps_needed depth_remaining visit in
    non || pref


let ilds_bottom_brancher n info discreps_needed depth_remaining visit =
  let pref = visit_preferred n info discreps_needed depth_remaining visit in
  let non = visit_non_preferred n info discreps_needed depth_remaining visit in
    pref || non


let ilds_iteration node info max_depth brancher num_discreps =
  (** returns true when tree is exhasuted *)
  let used_all = ilds_iteration node info max_depth num_discreps brancher in
    if (num_discreps = max_depth)
    then true
    else (not used_all)


let iterated_ilds
    (* returns optional node, stats, optimal (does sol satisfy optimal_p),
       complete (was entire space searched) *)
    ?(optimal_p = (Fn.constantly1 false))
    ?(prune_p = (Fn.constantly2 false))
    ?(prev_best = None)
    ?(halt = [Info.Never])
    ?(log = Fn.no_op2)
    ?(concurrent = 10)
    copy_state
    max_depth
    better_p
    leaf_p
    num_children
    get_child
    initial =
  let limits = Info.split_limit (List.hd halt) concurrent
  and info = (Info.make num_children get_child leaf_p better_p optimal_p
		copy_state prune_p log prev_best halt)
  and brancher = ilds_top_brancher
  and complete = ref false in
    for i = 0 to (concurrent - 1)
    do
      (info.Info.halt_p <- Info.make_halt_p [limits.(i)];
       Verb.pe Verb.always "Running %ith iteration of %i\n%!" i concurrent;
       complete := (!complete ||
		      (if Info.optimal_so_far info
		       then Info.leaf_or_prune_p info initial
		       else
			 try
			   ilds_iteration initial info max_depth brancher i
			 with Halted | Sys.Break -> false));)

    done;
    ((Info.curr_best info),
     (Info.stats info),
     (Info.optimal_so_far info),
     !complete)

(* EOF *)

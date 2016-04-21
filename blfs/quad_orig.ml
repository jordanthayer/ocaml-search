(* $Id: separate.ml,v 1.2 2004/01/11 01:24:30 ruml Exp ruml $

   Quadratic per child rank

   Uses a Separate.t data structure.  The "saved" field holds the
   table used for bound estimation and search.  The parameters hold
   the coefficients.  In both, the last value holds the constant.
*)

open Separate


(************* initialization before blfs *****************)


let clear_pattern p =
  let l = Array.length p in
    Array.fill p 0 l 0.;
    p.(l - 1) <- 1.


let random_probe m get_cost info root =
  (** returns true iff reached a leaf *)
  let d_factor = 1. /. (float (max_depth m))
  and p = m.pattern
  in
  let rec visit n depth =
    if (info#check_halt)						(* TODO *)
    then raise Halted
    else if (info#leaf_p n)						(* TODO *)
    then (m.term.(depth) <- m.term.(depth) + 1;
	  if (info#check_best_and_optimal n)
	  then raise Halted;
	  m.show_ex (get_cost n);
	  true)
    else if (info#prune_p n)						(* TODO *)
    then (m.term.(depth) <- m.term.(depth) + 1;
	  false)
    else (info#incr_branches;						(* TODO *)
	  m.non_term.(depth) <- m.non_term.(depth) + 1;
	  let child, index = Separate.get_random_child info n m.max_children in
	    if index > 0 then
	      (let p_base = (index - 1) * 3
	       and df = (float depth) *. d_factor in
		 p.(p_base) <- p.(p_base) +. (df *. df);
		 p.(p_base + 1) <- p.(p_base + 1) +. df;
		 p.(p_base + 2) <- p.(p_base + 2) +. 1.);
	    visit child (depth + 1))
  in
    clear_pattern p;
    visit root 0


let random_probes model get_cost info initial num_probes =
  try
    let successes = ref 0 in
      while !successes < num_probes do
	if (random_probe model get_cost info initial)
	then incr successes
      done
  with Halted -> ()


(************* reiniting model between passes *****************)


let prepare_for_search m =
  (** assumes that fill_saved was called before estimating bound *)
  Wrarray.fill_all m.non_term 0;
  Wrarray.fill_all m.term 0;
  (* may have garbage if previous iteration was halted *)
  clear_pattern (params m)


let initial_data m _ =
  prepare_for_search m;
  (Wrarray.last (params m)), 0


(************** using model to choose children ************)


let map_children m =
  let d_factor = 1. /. (float (max_depth m))
  and max_c = m.max_children
  and p = m.pattern
  and costs = m.saved in
    (fun do_child notify_skipping n (so_far, depth) bound ->
       (** controls which children blfs will take.  assumes that costs
	 have been adjusted so that completion cost is zero, and so
	 costs are non-decreasing with child rank. *)
       m.non_term.(depth) <- m.non_term.(depth) + 1;
       let base = depth * max_c
       and next_depth = depth + 1 in
	 (* always visit 0, no pattern additions *)
	 do_child n 0 (so_far +. costs.(base), next_depth);
	 let df = (float depth) *. d_factor in
	 let df2 = df *. df in
	   for i = 1 to max_c - 1 do
	     let cost = so_far +. costs.(base + i) in
	       if cost <= bound then
		 let p_base = (i - 1) * 3 in
		 let a = p.(p_base)
		 and b = p.(p_base + 1)
		 and c = p.(p_base + 2) in
		   p.(p_base) <- a +. df2;
		   p.(p_base + 1) <- b +. df;
		   p.(p_base + 2) <- c +. 1.;
		   do_child n i (cost, next_depth);
		   p.(p_base) <- a;
		   p.(p_base + 1) <- b;
		   p.(p_base + 2) <- c;
	       else
		 notify_skipping ()
	   done)


(************** estimating the cost bound ************)


let fill_saved m =
  (** saved is a table of precomputed costs.  will have 0. for child
    0, but not necessarily monotonic w/ rank! *)
  let p = params m
  and s = m.saved
  and max_d = (max_depth m)
  and max_c = m.max_children in
    for d = 0 to max_d - 1 do
      let s_base = (d * max_c) + 1
      and df = Math.div d max_d in
      let df2 = df *. df in
	for i = 0 to max_c - 2 do
	  let p_base = i * 3 in
	  let a = p.(p_base)
	  and b = p.(p_base + 1)
	  and c = p.(p_base + 2) in
	    s.(s_base + i) <- (a *. df2) +. (b *. df) +. c
	done;
    done


let bound_for_nodes m nodes =
  (** return threshold for next pass *)
  if (nodes < 1) then (write_params "saved params" m.saved;
		       write_params "current params" (params m);
		       failwith "negative number of nodes desired!");
  fill_saved m;
  let costs = m.saved in
  let get_child_cost depth child max_cost =
    let start = depth * m.max_children in
    let this = costs.(start + child) in
      if this <= max_cost
      then Some (init_hist m.max_bins this)
      else None
  in
    Separate.find_bound_for_nodes (max_depth m) m.max_children m.max_bins
      (Wrarray.last (params m)) get_child_cost m.non_term m.term nodes


(********** packaging up the interface *************)


let make_model max_depth max_children max_bins learner =
  let n = (3 * (max_children - 1)) + 1 in
  let pat = Array.make n 0. in
  let show, _, get = learner n
  in
    { non_term = Array.make (max_depth + 1) 0;
      term = Array.make (max_depth + 1) 0;
      max_children = max_children;
      pattern = pat;
      saved = Array.make (max_depth * max_children) 0.;
      show_ex = (fun cost -> ignore (show pat cost));
      get_params = get;
      max_bins = max_bins; }


let make_blfs_interface max_depth max_children max_bins learner get_leaf_cost =
  let m = make_model max_depth max_children max_bins learner in
  let i = Blfs.make_interface
	    (initial_data m)
	    (Separate.make_update_leaf m get_leaf_cost)
	    (Separate.update_term m)
	    (map_children m)
	    (bound_for_nodes m)
  in i, m


(************* integration with BLFS *****************)


let blfs_quad max_depth max_children get_leaf_cost
  ?(learner = Lms.init_lms)
  ?(max_bins = 200)
  ?(num_probes = max_children * 5)
  ?(optimal_p = (Wrutils.constantly1 false))
  ?(prune_p = (Wrutils.constantly2 false))
  ?(prev_best = None)
  ?(halt = Info.Never)
  ?(log = Wrutils.no_op2)
  copy_state
  better_p
  leaf_p
  get_child
  initial
  (** nodes at [max_depth] will never have children (root is at depth
    0). *)
  =
  let info = new Info.basic get_child leaf_p better_p optimal_p copy_state
	       prune_p log prev_best halt
  and interface, model = make_blfs_interface max_depth max_children max_bins
			   learner get_leaf_cost in
    if info#optimal_so_far					(* TODO *)
    then info#curr_best, info#curr_stats, true, (info#leaf_or_prune_p initial) (* TODO *)
    else
      (random_probes model get_leaf_cost info initial num_probes;
       Blfs.best_leaf_first_search interface info 0. initial)


(* EOF *)

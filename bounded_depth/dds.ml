(* $Id: dds.ml,v 1.1 2003/07/17 22:40:28 ruml Exp ruml $

   depth-limited discrepancy search

   treats all non-preferred children equally
*)

exception Optimal

exception Halted


let iteration info root threshold =
  let rec visit n depth =
    (** returns max depth of nodes *)
    if (Info.leaf_p info n) then
      (if (Info.optimal_p info n) then
	 raise Optimal;
       depth)
    else if (Info.prune_p info n) then
      depth
    else if (Info.halt_p info) then
      raise Halted
    else
      (Info.incr_branches info;
       if (depth > threshold) then
	 visit (Info.get_child info n 0) (depth + 1)
       else if (depth < threshold) then
	 iter_children n depth 0
       else
	 (* skip preferred child at threshold *)
	 iter_children n depth 1)
  and iter_children n depth start =
    let next = depth + 1
    and max = ref depth in
      for i = start to (Info.num_children info n) - 1 do
	let d = visit (Info.get_child info n i) next in
	  if d > !max then max := d
      done;
      !max
  in
    visit root 0


let search
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
  =
  let info = Info.make num_children get_child leaf_p better_p optimal_p
	       copy_state prune_p log prev_best halt in
    (* depth after which we start always choosing preferred child.  at
       this depth, always take other children only, since we took the
       preferred path on previous iteration *)
  let threshold = ref (-1)
  and max_depth = ref 0
  and optimal = ref (Info.optimal_so_far info)
  and complete = ref false in
    if not !optimal
    then (try
	    while (not !complete) do
	      let est_depth = iteration info initial !threshold in
		if est_depth > !max_depth
		then max_depth := est_depth;
		(* have visited all nodes at level after threshold *)
		if !threshold = (!max_depth - 1)
		then complete := true
		else incr threshold
	    done
	  with
	    Halted -> ()
	  | Optimal -> optimal := true);
    (Info.curr_best info), (Info.stats info), !optimal, !complete



let test () =
  search
    ~optimal_p:Bounded_depth_test.optimal_p
    Fn.identity
    Bounded_depth_test.better_p
    Bounded_depth_test.leaf_p
    Bounded_depth_test.num_children
    Bounded_depth_test.get_child
    (Bounded_depth_test.make_initial 2 3)


(* EOF *)

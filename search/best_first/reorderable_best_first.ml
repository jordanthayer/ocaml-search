(** Best first searches which allow for the reordering of
    nodes in their open list, possible at every expansion

    Jordan - July 2009 *)

let no_record = (fun _ _ -> ())

let closed_pos = -2

let init_lists pred key hash equal setpos initial =
  let openlist = Dpq.create pred setpos 100 initial
  and nodes = Htable.create hash equal 100 in
    Dpq.insert openlist initial;
    Htable.add nodes (key initial) initial;
    openlist, nodes

let search ?(record = no_record) sface ordered_p better_p update_open =
  (** Needs some commenting *)
  let q = Dpq.create_with ordered_p sface.Search_interface.initial
  and i = sface.Search_interface.info in
    Dpq.insert q sface.Search_interface.initial;
    let rec expand_best () =
      if (not (Dpq.empty_p q)) && (not (Limit.halt_p i))
      then
	(record i q;
	 let n = Dpq.extract_first q in
	   if not (Limit.promising_p i n) then
	     (Limit.incr_prune i;
	      expand_best ())
	   else if sface.Search_interface.goal_p n then
	     Limit.new_incumbent i (Limit.Incumbent (0.,n))
	   else
	     (Limit.incr_exp i;
	      let reorder, children = sface.Search_interface.resort_expand n in
		if reorder then update_open q;
		List.iter (fun n ->
			     Limit.incr_gen i;
			     if Limit.promising_p i n then
			       Dpq.insert q n
			     else
			       Limit.incr_prune i)
		  children;
		Limit.curr_q i (Dpq.count q);
		expand_best ())) in
      expand_best ();
      Limit.results5 i


let search_dups ?(record = no_record) sface ordered_p better_p setpos getpos
    update_open =
  (** [ordered_p] is true if in order or equal, [better_p] is true if
      superior or equal (eg, for comparing duplicates), [key] gives
      state data for detecting duplicates, [setpos] and [getpos] are
      for efficient swapping into the openlist.  This function is
      intended to be a building block for others - it expects an
      [expand] that takes the openlist as the first argument (in case
      [expand] wants to mess with it). *)
  (* openlist reflects ordering.  nodes has all nodes.  pos is [closedpos]
     for nodes on the closed list. *)
  let openlist, nodes = init_lists ordered_p sface.Search_interface.key
    sface.Search_interface.hash sface.Search_interface.equals setpos
    sface.Search_interface.initial
  and i = sface.Search_interface.info in
  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n) then
      Limit.incr_prune i
    else
      let state = sface.Search_interface.key n in
	(* if heuristic is consistent (monotone) then
	   first instance of node is always best path. *)
	try
	  let prev = Htable.find nodes state in
	    Limit.incr_dups i;
	    if not (better_p prev n) then
	      (* better path to previous state *)
	      (Htable.replace nodes state n;
	       let pos = getpos prev in
		 if pos == closed_pos
		 then Dpq.insert openlist n
		 else Dpq.swap openlist pos n)
	with Not_found -> (* new state *)
	  Dpq.insert openlist n;
	  Htable.add nodes state n in
  let rec expand_best () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i))
    then
      (record i openlist;
       let n = Dpq.extract_first openlist in
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    Htable.remove nodes (sface.Search_interface.key n);
	    expand_best ())
	 else if sface.Search_interface.goal_p n then
	   Limit.new_incumbent i (Limit.Incumbent (0.,n))
	 else
	   (setpos n closed_pos;
	    Limit.incr_exp i;
	    let reorder, children = sface.Search_interface.resort_expand n in
	      if reorder then update_open openlist;
	      List.iter consider_child
		children;
	      Limit.curr_q i (Dpq.count openlist);
	      expand_best ()))
  in
    expand_best ();
    Limit.results6 i


let search_drop_dups ?(record = no_record) sface ordered_p better_p
    setpos getpos update_open =
  (** [pred] is true if in order or equal (both for expansion and for
      testing which duplicate is superior), [key] gives state data for
      detecting duplicates, [setpos] and [getpos] are for efficient swapping
      into the openlist.  *)
  (* openlist reflects ordering.  nodes has all nodes.  pos is [closedpos]
     for nodes on the closed list. *)
  let openlist, nodes = init_lists ordered_p sface.Search_interface.key
    sface.Search_interface.hash sface.Search_interface.equals setpos
    sface.Search_interface.initial
  and i = sface.Search_interface.info in
  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n) then
      Limit.incr_prune i
    else
      let state = sface.Search_interface.key n in
	(* if heuristic is consistent (monotone) then
	   first instance of node is always best path. *)
	try
	  let prev = Htable.find nodes state in
	    Limit.incr_dups i;
	    if not (better_p prev n) then
	      (* better path to previous state *)
	      (let pos = getpos prev in
		 if not (pos == Dpq.no_position) then
		   (Dpq.swap openlist pos n;
		    Htable.replace nodes state n))
	with Not_found ->
	  (* new state *)
	  Dpq.insert openlist n;
	  Htable.add nodes state n in
  let rec expand_best () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i)) then
      (record i openlist;
       let n = Dpq.extract_first openlist in
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    (* any future duplicate will be pruned by bound *)
	    Htable.remove nodes (sface.Search_interface.key n);
	    expand_best ())
	 else if sface.Search_interface.goal_p n then
	   Limit.new_incumbent i (Limit.Incumbent (0., n))
	 else
	   (setpos n Dpq.no_position;
	    Limit.incr_exp i;
	    let reorder, children = sface.Search_interface.resort_expand n in
	      if reorder then update_open openlist;
	      List.iter consider_child children;
	      Limit.curr_q i (Dpq.count openlist);
	      expand_best ()))
  in
    expand_best ();
    Limit.results6 i


(* EOF *)

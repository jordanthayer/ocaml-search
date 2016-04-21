(** Anytime Searches of the continued variety.
    Searches based on some node ordering, then continues until time out,
    or all nodes have been pruned.
    At each new goal, re-order the nodes based on a weightlist
    Jordan - July 2009 *)


let no_dups sface ordered_p wtlist updater =
  (** Continued search for anytime search performance.
      [sface] is a search interface with a node expand
      [ordered_p] determines if nodes are in best first order
      [wtlist] a list of weights to be used in the search
      [upadter] Updates cost values of nodes when the weight changes *)
  let q = Dpq.create_with ordered_p sface.Search_interface.initial
  and i = sface.Search_interface.info
  and wts = ref (List.rev wtlist) in
    Dpq.insert q sface.Search_interface.initial;
    let rec expand_best () =
      if (not (Dpq.empty_p q)) && (not (Limit.halt_p i)) then
	let n = Dpq.extract_first q in
	  if not (Limit.promising_p i n) then
	    (Limit.incr_prune i;
	     expand_best ())
	  else if sface.Search_interface.goal_p n
	  then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		if (List.length !wts) > 1
		then
		  (wts := List.tl !wts;
		   (*Verb.pe Verb.debug "new wt %f\n" (List.hd !wts);*)
		   Dpq.iter (updater (List.hd !wts)) q;
		   Dpq.resort_old q);
		expand_best())
	  else
	    (Limit.incr_exp i;
	     let wt = List.hd !wts in
	       List.iter (fun n ->
			    updater wt n;
			    Limit.incr_gen i;
			    if Limit.promising_p i n then
			      Dpq.insert q n
			    else
			      Limit.incr_prune i)
		 (sface.Search_interface.node_expand n);
	       Limit.curr_q i (Dpq.count q);
	       expand_best ()) in
      expand_best ();
      Limit.results5 i


let closed_pos = -2
and delayed = -3

let init_lists pred sface setpos =
  let key = sface.Search_interface.key
  and initial = sface.Search_interface.initial in
  let openlist = Dpq.create pred setpos 100 initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100 in
    Dpq.insert openlist initial;
    Htable.add nodes (key initial) initial;
    openlist, nodes


let dups sface ordered_p better_p setpos getpos wtlist updater =
  (** [ordered_p] is true if in order or equal, [better_p] is true if
      superior or equal (eg, for comparing duplicates), [key] gives
      state data for detecting duplicates, [setpos] and [getpos] are
      for efficient swapping into the openlist.  This function is
      intended to be a building block for others - it expects an
      [expand] that takes the openlist as the first argument (in case
      [expand] wants to mess with it). *)
  (* openlist reflects ordering.  nodes has all nodes.  pos is [closedpos]
     for nodes on the closed list. *)
  let openlist, nodes = init_lists ordered_p sface setpos
  and i = sface.Search_interface.info
  and wts = ref (List.rev wtlist) in
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
      (let n = Dpq.extract_first openlist in
	 setpos n closed_pos;
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    Htable.remove nodes (sface.Search_interface.key n);
	    expand_best ())
	 else if sface.Search_interface.goal_p n
	 then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	       if (List.length !wts) > 1
	       then (wts := List.tl !wts;
		     Dpq.iter (updater (List.hd !wts)) openlist;
		     Dpq.resort_old openlist);
	       expand_best ())
	 else
	   (Limit.incr_exp i;
	    let wt = List.hd !wts in
	      List.iter (fun n -> updater wt n; consider_child n)
		(sface.Search_interface.node_expand n);
	      Limit.curr_q i (Dpq.count openlist);
	      expand_best ()))
  in
    expand_best ();
    Limit.results6 i


let delay_dups sface ordered_p better_p setpos getpos wtlist updater =
  (** [pred] is true if in order or equal (both for expansion and for
      testing which duplicate is superior), [key] gives state data for
      detecting duplicates, [setpos] and [getpos] are for efficient swapping
      into the openlist.  *)
  let openlist, nodes = init_lists ordered_p sface setpos
  and delay = Dpq.create_with ordered_p sface.Search_interface.initial
  and i = sface.Search_interface.info
  and wts = ref (List.rev wtlist) in
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
		 if not (pos == closed_pos)
		 then (Dpq.swap openlist pos n;
		       Htable.replace nodes state n)
		 else (Dpq.insert delay n;
		       setpos n delayed))
	with Not_found ->
	  (* new state *)
	  Dpq.insert openlist n;
	  Htable.add nodes state n in
  let rec expand_best () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i)) then
      (let n = Dpq.extract_first openlist in
	 setpos n closed_pos;
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    (* any future duplicate will be pruned by bound *)
	    Htable.remove nodes (sface.Search_interface.key n);
	    expand_best ())
	 else if sface.Search_interface.goal_p n
	 then (Limit.new_incumbent i (Limit.Incumbent (0., n));
	       while (not (Dpq.empty_p delay))
	       do
		 Dpq.insert openlist (Dpq.extract_first delay)
	       done;
	       if (List.length !wts) > 1
	       then
		 (wts := List.tl !wts;
		  Dpq.iter (updater (List.hd !wts)) openlist;);
	       expand_best ())
	 else
	   (Limit.incr_exp i;
	    let wt = List.hd !wts in
	      List.iter
		(fun n -> updater wt n;
		   consider_child n) (sface.Search_interface.node_expand n);
	      Limit.curr_q i (Dpq.count openlist);
	      expand_best ()))
  in
    expand_best ();
    Limit.results6 i



(* EOF *)

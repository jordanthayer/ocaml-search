(** Anytime Searches of the continued variety.
    Searches based on some node ordering, then continues until time out
    or all nodes have been pruned.
    Jordan - July 2009 *)


let no_record = (fun _ _ -> ())


let no_dups ?(record = no_record) sface ordered_p =
  (** Continued search for anytime search performance.
      [sface] is a search interface with a node expand
      [ordered_p] determines if nodes are in best first order *)
  let q = Dpq.create_with ordered_p sface.Search_interface.initial
  and i = sface.Search_interface.info in
  let rec expand_best () =
    if (not (Dpq.empty_p q)) && (not (Limit.halt_p i))
    then
      (record i q;
       let n = Dpq.extract_first q in
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    expand_best ())
	 else if sface.Search_interface.goal_p n
	 then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	       expand_best())
	 else
	   (Limit.incr_exp i;
	    List.iter (fun n ->
			 Limit.incr_gen i;
			 if Limit.promising_p i n
			 then Dpq.insert q n
			 else Limit.incr_prune i)
	      (sface.Search_interface.node_expand n);
	    Limit.curr_q i (Dpq.count q);
	    expand_best ())) in
    Dpq.insert q sface.Search_interface.initial;
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


let dups ?(record = no_record) sface better_p ordered_p setpos getpos =
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
	 setpos n closed_pos;
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    Htable.remove nodes (sface.Search_interface.key n);
	    expand_best ())
	 else (if sface.Search_interface.goal_p n
	       then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		     expand_best ())
	       else
		 (Limit.incr_exp i;
		  List.iter consider_child
		    (sface.Search_interface.node_expand n);
		  Limit.curr_q i (Dpq.count openlist);
		  expand_best ())))
  in
    expand_best ();
    Limit.results6 i


let delay_dups ?(record = no_record) sface ordered_p better_p setpos getpos =
  (** [pred] is true if in order or equal (both for expansion and for
      testing which duplicate is superior), [key] gives state data for
      detecting duplicates, [setpos] and [getpos] are for efficient swapping
      into the openlist.  *)
  (* openlist reflects ordering.  nodes has all nodes.  pos is [closedpos]
     for nodes on the closed list. *)
  let openlist, nodes = init_lists ordered_p sface setpos
  and delay = Dpq.create_with ordered_p sface.Search_interface.initial
  and i = sface.Search_interface.info in

  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else (let state = sface.Search_interface.key n in
	  try (let prev = Htable.find nodes state in
	       Limit.incr_dups i;
	       if not (better_p prev n) (* better path to previous state *)
	       then (let pos = getpos prev in
		     if not (pos = Dpq.no_position)
		     then (Dpq.swap openlist pos n;
			   Htable.replace nodes state n)
		     else (Dpq.insert delay n;
			   Htable.replace nodes state n)))
	  with Not_found -> 	    (* new state *)
	    Dpq.insert openlist n;
	    Htable.add nodes state n) in

  let dump_delayed () =
    Verb.pe Verb.always "dumping delayed nodes...";
    while (not (Dpq.empty_p delay))
    do
      (let n = Dpq.extract_first delay in
       let state = sface.Search_interface.key n in
       let prev = Htable.find nodes state in
       assert ((getpos n) = Dpq.no_position);
       if (Limit.promising_p i n) && prev == n
       then Dpq.insert openlist n)
    done;
    Verb.pe Verb.always "done!\n%!" in

  let rec expand_best () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i))
    then (let n = Dpq.extract_first openlist in
	  setpos n Dpq.no_position;
	  if not (Limit.promising_p i n)
	  then (Limit.incr_prune i;
		Htable.remove nodes (sface.Search_interface.key n);
		expand_best ())
	  else if sface.Search_interface.goal_p n
	  then (Verb.pe Verb.always "new goal ";
		Limit.new_incumbent i (Limit.Incumbent (0., n));
		dump_delayed();
		expand_best ())
	  else (setpos n Dpq.no_position;
		Limit.incr_exp i;
		List.iter consider_child (sface.Search_interface.node_expand n);
		Limit.curr_q i (Dpq.count openlist);
		expand_best ()))
    else (if (not (Dpq.empty_p delay)) && (not (Limit.halt_p i))
          then (Verb.pe Verb.always "empty open ";
		dump_delayed ();
		expand_best()))
  in
  expand_best ();
  Limit.results6 i

(* EOF *)

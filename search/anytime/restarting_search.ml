(** Restarts Weighted A* in the Search Framework

    Jordan - July 2009 *)


let no_record = (fun _ _ -> ())

let init_lists pred sface setpos =
  let key = sface.Search_interface.key
  and initial = sface.Search_interface.initial in
  let openlist = Dpq.create pred setpos 100 initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100 in
    Dpq.insert openlist initial;
    Htable.add nodes (key initial) initial;
    openlist, nodes

let closed_pos = -2
and prev_it = -3


let search  ?(record = no_record) sface initial wts ordered_p feasible better_p
    setpos getpos max_it update =
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
  and iteration = ref 0 in
  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else
      let state = sface.Search_interface.key n in
	try
	  let prev = Htable.find nodes state in
	    Limit.incr_dups i;
	    let pos = getpos prev in
	      (* Recalculate the w values you fucking jackass.
		 What the hell is wrong with you. *)
	      if not (better_p prev n)
	      then
		(Htable.replace nodes state n;
		 if pos <> closed_pos && pos <> prev_it
		 then Dpq.swap openlist pos n
		 else Dpq.insert openlist n)
	      else if pos == prev_it
	      then
		(if better_p prev n
		 then (update prev (List.nth wts !iteration);
		       Dpq.insert openlist prev)
		 else (Htable.replace nodes state n;
		       Dpq.insert openlist n))
	with Not_found ->
	  (* new state *)
	  Htable.add nodes state n;
	  Dpq.insert openlist n in

  let rec expand_best () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i)) then
      (record i openlist;
       let n = Dpq.extract_first openlist in
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    (*future duplicate pruned by bound *)
	    Htable.remove nodes (sface.Search_interface.key n);
	    expand_best ())
	 else if sface.Search_interface.goal_p n then
	   (match i.Limit.incumbent with
		Limit.Nothing ->
		  (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		   Dpq.clear openlist;
		   Htable.iter (fun _ n -> setpos n prev_it) nodes;
		   Dpq.insert openlist initial;
		   iteration := !iteration + 1;
		   if !iteration < max_it
		   then expand_best())
	      | Limit.Incumbent (qual,node) ->
		  Limit.new_incumbent i (Limit.Incumbent (0.,n));
		  Dpq.clear openlist;
		  Htable.iter (fun _ n -> setpos n prev_it) nodes;
		  Dpq.insert openlist initial;
		  (* new itteration only begins on truly new incumbent*)
		  if better_p n node then iteration := !iteration + 1;
		  if !iteration < max_it
		  then expand_best())
	 else
	   (setpos n closed_pos;
	    Limit.incr_exp i;
	    List.iter (fun c ->
			 update c (List.nth wts !iteration);
			 consider_child c)
	      (sface.Search_interface.node_expand n);
	    Limit.curr_q i (Dpq.count openlist);
	    expand_best ()))
  in
    expand_best ();
    i


let no_dups sface wts ordered_p feasible better_p
    setpos getpos max_it update =
  (** Performs restarting search on domains with few or no duplicate states
      [sface]  search interface to nodes
      [wts] lits of weights to be used, in order
      [ordered_p] ordered predicate
      [feasible] can this node ever produce a better incumbent
      [better_p] is this a better solution
      [setpos] updates nodes position in queue
      [getpos] returns nodes position in queue
      [max_it] maximum number of iterations to perform
      [update] updates the cost of a node based on current weight *)
  Limit.results5 (search sface sface.Search_interface.initial wts ordered_p
		    feasible better_p setpos getpos max_it update)


and dups sface wts ordered_p feasible better_p
    setpos getpos max_it update =
  (** Performs restarting search on domains with few or no duplicate states
      [sface]  search interface to nodes
      [wts] lits of weights to be used, in order
      [ordered_p] ordered predicate
      [feasible] can this node ever produce a better incumbent
      [better_p] is this a better solution
      [setpos] updates nodes position in queue
      [getpos] returns nodes position in queue
      [max_it] maximum number of iterations to perform
      [update] updates the cost of a node based on current weight *)
  Limit.results6 (search sface sface.Search_interface.initial wts ordered_p
		    feasible better_p setpos getpos max_it update)


let delay_dups sface wts ordered_p feasible better_p
    setpos getpos max_it update =
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
  and iteration = ref 0 in
  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else
      let state = sface.Search_interface.key n in
	try
	  let prev = Htable.find nodes state in
	    Limit.incr_dups i;
	    let pos = getpos prev in

	      (* Recalculate the w values you fucking jackass.
		 What the hell is wrong with you. *)

	      if not (feasible prev n)
	      then
		(Htable.replace nodes state n;
		 if pos <> closed_pos && pos <> prev_it
		 then Dpq.swap openlist pos n
		 else (if pos == prev_it
		       then Dpq.insert openlist n))
	      else (if pos == prev_it
		    then (if better_p prev n
			  then (update prev (List.nth wts !iteration);
				Dpq.insert openlist prev)
			  else (Htable.replace nodes state n;
				Dpq.insert openlist n)))


	with Not_found ->
	  (* new state *)
	  Htable.add nodes state n;
	  Dpq.insert openlist n in

  let rec expand_best () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i)) then
      let n = Dpq.extract_first openlist in
	if not (Limit.promising_p i n) then
	  (Limit.incr_prune i;
	   (*future duplicate pruned by bound *)
	   Htable.remove nodes (sface.Search_interface.key n);
	   expand_best ())
	else if sface.Search_interface.goal_p n then
	  (match i.Limit.incumbent with
	       Limit.Nothing ->
		 (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		  Dpq.clear openlist;
		  Htable.iter (fun _ n -> setpos n prev_it) nodes;
		  Dpq.insert openlist sface.Search_interface.initial;
		  iteration := !iteration + 1;
		  if !iteration < max_it
		  then expand_best())
	     | Limit.Incumbent (qual,node) ->
		 Limit.new_incumbent i (Limit.Incumbent (0.,n));
		 Dpq.clear openlist;
		 Htable.iter (fun _ n -> setpos n prev_it) nodes;
		 Dpq.insert openlist sface.Search_interface.initial;
		 if feasible n node
		 then iteration := !iteration + 1;
		 if !iteration < max_it
		 then expand_best())
	else
	  (setpos n closed_pos;
	   Limit.incr_exp i;
	   List.iter (fun c -> update c (List.nth wts !iteration);
			consider_child c)
	     (sface.Search_interface.node_expand n);
	   Limit.curr_q i (Dpq.count openlist);
	   expand_best ())
  in
    expand_best ();
    Limit.results6 i



(* EOF *)

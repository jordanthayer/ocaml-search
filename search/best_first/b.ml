(** B-styled Searches as proposed by Martelli in 1977 *)

let no_record = (fun _ _ -> ())
and closed_pos = -2


let init_lists pred sface setpos =
  let key = sface.Search_interface.key
  and initial = sface.Search_interface.initial in
  let openlist = Dpq.create pred setpos 100 initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100 in
    Dpq.insert openlist initial;
    Htable.add nodes (key initial) initial;
    openlist, nodes


let get_min_g q max_best get_cost get_g get_pos =
  (** Extracts the node with cost < max_best with minimum g.
      Assumes q is ordered on cost *)
  let unsafe_iterator,reset = Dpq.make_iterator_unsafe q in
  let rec do_it current =
    match unsafe_iterator () with
	None -> current
      | Some n -> (if (get_cost n) > max_best
		   then current
		   else (if (get_g n) < (get_g current)
			 then do_it n
			 else do_it current)) in
  let n = do_it (Dpq.peek_first q) in
    Dpq.remove q (get_pos n);
    n


let search ?(record = no_record) sface ordered_best get_cost get_g
    set_pos get_pos =
  (** Performs a B styled search on nodes sorted on cost, which might be f *)
  let q = Dpq.create ordered_best set_pos 100 sface.Search_interface.initial
  and i = sface.Search_interface.info
  and max_best = ref neg_infinity in
  let rec expand_best () =
    if (not (Dpq.empty_p q)) && (not (Limit.halt_p i))
    then
      (record i q;
       let n = (if (get_cost (Dpq.peek_first q)) >= !max_best
		then (max_best := (get_cost (Dpq.peek_first q));
		      Dpq.extract_first q)
		else get_min_g q !max_best get_cost get_g get_pos) in
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    expand_best ())
	 else if sface.Search_interface.goal_p n then
	   Limit.new_incumbent i (Limit.Incumbent (0.,n))
	 else
	   (Limit.incr_exp i;
	    (try
	       List.iter (fun n ->
			    Limit.incr_gen i;
			    if Limit.promising_p i n then
			      Dpq.insert q n
			    else
			      Limit.incr_prune i)
		 (sface.Search_interface.node_expand n);
	     with _ -> failwith "Best_first.search");
	    Limit.curr_q i (Dpq.count q);
	    expand_best ())) in
    Dpq.insert q sface.Search_interface.initial;
    expand_best ();
    Limit.results5 i



let search_dups ?(record = no_record) sface ordered_p better_p
    get_cost get_g getpos setpos =
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
  and max_best = ref neg_infinity in
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
       let n = (if (get_cost (Dpq.peek_first openlist)) >= !max_best
		then (max_best := (get_cost (Dpq.peek_first openlist));
		      Dpq.extract_first openlist)
		else get_min_g openlist !max_best get_cost get_g getpos) in
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    Htable.remove nodes (sface.Search_interface.key n);
	    expand_best ())
	 else if sface.Search_interface.goal_p n then
	   Limit.new_incumbent i (Limit.Incumbent (0.,n))
	 else
	   (setpos n closed_pos;
	    let children = sface.Search_interface.node_expand n in
	      Limit.incr_exp i;
	      List.iter consider_child children;
	      Limit.curr_q i (Dpq.count openlist);
	      expand_best ()))
  in
    expand_best ();
    Limit.results6 i

(* EOF *)

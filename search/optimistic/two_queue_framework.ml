(** Much like the optimistic framework, but does not delay creating the
    cleanup queue *)

let no_record = (fun _ _ _ -> ())
exception Done

let search ?(record = no_record) ordered_agg ordered_clean
    set_aq_pos set_cq_pos get_node i sface bound =
  let agg_queue = Dpq.create ordered_agg set_aq_pos 100
    sface.Search_interface.initial
  and clean_queue = Dpq.create ordered_clean set_cq_pos 100
    sface.Search_interface.initial in

  let insert_node n =
    Dpq.insert agg_queue n;
    Dpq.insert clean_queue n in

  let rec expand () =
    if not (Dpq.empty_p agg_queue) && (not (Limit.halt_p i))
    then
      (record i agg_queue clean_queue;
       let next = get_node agg_queue clean_queue i in
	 if not (Limit.promising_p i next)
	 then (Limit.incr_prune i;
	       expand ())
	 else (if sface.Search_interface.goal_p next
	       then (Limit.new_incumbent i (Limit.Incumbent (0.,next));
		     expand())
	       else
		 (Limit.incr_exp i;
		  List.iter (fun c ->
			       Limit.incr_gen i;
			       if Limit.promising_p i c
			       then insert_node c
			       else Limit.incr_prune i)
		    (sface.Search_interface.node_expand next);
		  Limit.curr_q i (Dpq.count agg_queue);
		  expand ()))) in
    insert_node sface.Search_interface.initial;
    (try
       expand()
     with Done -> (match i.Limit.incumbent with
		       Limit.Nothing -> failwith "Done but no incumbent?"
		     | Limit.Incumbent (q,n) ->
			 Limit.new_incumbent i (Limit.Incumbent (bound,n))));
    Limit.results5 i


let search_dups ?(record = no_record) ordered_agg ordered_clean better_p
    set_aq_pos set_cq_pos get_aq_pos get_cq_pos get_node i sface bound =
  let agg_queue = Dpq.create ordered_agg set_aq_pos 100
    sface.Search_interface.initial
  and clean_queue = Dpq.create ordered_clean set_cq_pos 100
    sface.Search_interface.initial
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100 in

  let insert_node n =
    let state = sface.Search_interface.key n in
      try
	let prev = Htable.find closed state in
	  Limit.incr_dups i;
	  if not (better_p prev n)
	  then (Htable.replace closed state n;
		if (get_aq_pos prev) = Dpq.no_position
		then (Dpq.insert agg_queue n;
		      Dpq.insert clean_queue n)
		else (Dpq.swap agg_queue (get_aq_pos prev) n;
		      Dpq.swap clean_queue (get_cq_pos prev) n))
      with Not_found ->
	(*Verb.pe Verb.debug "Inserting new node\n";*)
	Dpq.insert agg_queue n;
	Dpq.insert clean_queue n;
	Htable.add closed state n in

  let rec expand () =
    if not (Dpq.empty_p agg_queue) && (not (Limit.halt_p i))
    then
      (record i agg_queue clean_queue;
       let next = get_node agg_queue clean_queue i in
	 if not (Limit.promising_p i next)
	 then (Limit.incr_prune i;
	       expand ())
	 else (if sface.Search_interface.goal_p next
	       then (Limit.new_incumbent i (Limit.Incumbent (0.,next));
		     expand ())
	       else
		 (Limit.incr_exp i;
		  List.iter (fun c ->
			       Limit.incr_gen i;
			       if Limit.promising_p i c
			       then insert_node c
			       else Limit.incr_prune i)
		    (sface.Search_interface.node_expand next);
		  Limit.curr_q i (Dpq.count agg_queue);
		  expand ()))) in
    insert_node sface.Search_interface.initial;
    (try
       expand()
     with Done ->(match i.Limit.incumbent with
		      Limit.Nothing -> failwith "Done but no incumbent?"
		    | Limit.Incumbent (q,n) ->
			Limit.new_incumbent i (Limit.Incumbent (bound,n))));
    Limit.results6 i

(* EOF *)

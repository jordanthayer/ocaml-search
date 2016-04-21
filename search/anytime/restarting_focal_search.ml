(** Restarting Focal List Based Search Algorithms
    Things like A* epsilon for example
    Jordan - Feb 2010
*)

let no_record = (fun _ _ -> ())

let search ?(record = no_record) wtlist sface ordered1 make_close_enough
    ordered2 better_p setpos getpos =
  let wts = ref wtlist
  and openlist = Geq.create_with ordered1 ordered2
    (make_close_enough (List.hd wtlist)) setpos getpos
    sface.Search_interface.initial
  and i = sface.Search_interface.info in
    ignore (Geq.insert openlist sface.Search_interface.initial);
    let check_child n =
      Limit.incr_gen i;
      if Limit.promising_p i n then
	true
      else
	(Limit.incr_prune i;
	 false) in
    let geq_expand n =
      (** specify false return value iff found goal *)
      if not (Limit.promising_p i n) then
	(Limit.incr_prune i;
	 [], true)
      else if sface.Search_interface.goal_p n
      then
	(Limit.new_incumbent i (Limit.Incumbent (0., n));
	 Geq.clear openlist;
	 wts := List.tl !wts;
	 Geq.update_close_enough openlist (make_close_enough (List.hd !wts));
	 ignore (Geq.insert openlist sface.Search_interface.initial);
	 [], false)
      else
	(let children = sface.Search_interface.node_expand n in
	   (Limit.incr_exp i;
	    (List.filter check_child children), true)) in
    let rec do_loop () =
      if (not (Geq.empty_p openlist)) && (not (Limit.halt_p i))
	&& ((List.length !wts) > 0)
      then
	(record i openlist;
	 ignore (Geq.replace_using geq_expand Fn.no_op2 openlist);
	 Limit.curr_q i (Geq.count openlist);
	 do_loop ())
    in
      do_loop ();
      Limit.results5 i

let search_dups ?(record = no_record) wtlist sface ordered1 make_close_enough
    ordered2 better_p setpos getpos =
  (** take [key] as additional argument *)
  (* like a_star_dups, stores all nodes in hashtable [nodes],
     distinguishing the closed list by a special q_pos. note 3 kinds of node
     1) in openlist, garbage q_pos (init_pos or positive int)
     2) in openlist, valid q_pos (positive int)
     3) not in openlist, q_pos = closed_pos *)
  let closed_pos = -2
  and wts = ref wtlist
  and openlist = ref (Geq.create_with
			~equals:(fun a b ->
				   sface.Search_interface.equals
				     (sface.Search_interface.key a)
				     (sface.Search_interface.key b))
			ordered1 ordered2 (make_close_enough (List.hd wtlist))
			setpos getpos sface.Search_interface.initial)
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 250
  and i = sface.Search_interface.info in
  let next_geq wt = openlist :=
    (Geq.create_with
       ~equals:(fun a b ->
		  sface.Search_interface.equals
		    (sface.Search_interface.key a)
		    (sface.Search_interface.key b))
       ordered1 ordered2 (make_close_enough wt)
       setpos getpos sface.Search_interface.initial) in

  let insert_child n =
    (** return false iff [n] can be discarded *)
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else
      (let state = sface.Search_interface.key n in
	 try
	   let entry = Htable.find nodes state in
	     Limit.incr_dups i;
	     let prev = Geq.data entry in
	       if not (better_p prev n) then
		 (* better path to previous state. *)
		 (if (getpos prev) <> closed_pos
		    (* The current entry is somewhere in the geq *)
		  then (Geq.remove !openlist entry;
			setpos prev closed_pos;
			Htable.replace nodes state
			  (Geq.insert !openlist n))
		    (* The entry is not somewhere in the geq, we need to
		       re-insert it *)
		  else (Htable.replace nodes state
			  (Geq.insert !openlist n)))
	 with Not_found ->
	   Htable.add nodes state (Geq.insert !openlist n)) in

  let rec do_loop () =
    if ((not (Geq.empty_p !openlist)) && (not (Limit.halt_p i)))
    then
      (record i !openlist;
       let n = Geq.remove_best !openlist in
	 setpos n closed_pos;
	 (if (not (Limit.promising_p i n))
	  then Limit.incr_prune i
	  else (if sface.Search_interface.goal_p n
		then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
		      if List.length !wts > 1 then wts := List.tl !wts;
		      next_geq (List.hd !wts);
		      Htable.clear nodes;
		      Htable.add nodes (sface.Search_interface.key
					  sface.Search_interface.initial)
			(Geq.insert !openlist sface.Search_interface.initial))
		else
		  (Limit.incr_exp i;
		   List.iter insert_child
		     (sface.Search_interface.node_expand n))));
	 do_loop ())
  in
    Htable.add nodes
      (sface.Search_interface.key sface.Search_interface.initial)
      (Geq.insert !openlist sface.Search_interface.initial);
    do_loop ();
    Limit.results6 i

(* EOF *)

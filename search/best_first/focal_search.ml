(** Focal List Based Search Algorithms
    Things like A* epsilon for example
    Jordan - July 2009
*)

let no_record = (fun _ _ -> ())

let search ?(record = no_record) sface ordered1 close_enough ordered2 better_p
    setpos getpos =
  let openlist = Geq.create_with ordered1 ordered2 close_enough
			setpos getpos sface.Search_interface.initial
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
      (*Verb.pe Verb.debug "Top geq_expand: %f\n%!" (Sys.time() -. init);*)
      (** specify false return value iff found goal *)
      if not (Limit.promising_p i n) then
	(Limit.incr_prune i;
	 [], true)
      else if sface.Search_interface.goal_p n then
	(Limit.new_incumbent i (Limit.Incumbent (0., n));
	 [], false)
      else
	((*Verb.pe Verb.debug "Expanding: %f\n%!" (Sys.time() -. init);*)
	 let children = sface.Search_interface.node_expand n in
	   (Limit.incr_exp i;
	    (List.filter check_child children), true)) in
    let rec do_loop () =
      if (not (Geq.empty_p openlist)) && (not (Limit.halt_p i))
      then
	(record i openlist;
	 (*Verb.pe Verb.debug "Top of loop: %f\n%!" (Sys.time() -. init);*)
	 if (Geq.replace_using geq_expand Fn.no_op2 openlist) then
	   (Limit.curr_q i (Geq.count openlist);
	    do_loop ()))
    in
      do_loop ();
      Limit.results5 i


let search_dups ?(record = no_record) sface ordered1 close_enough
    ordered2 better_p setpos getpos =
  (* special q_pos values *)
  let closed_pos = -2 in
    (** take [key] as additional argument *)
    (* like a_star_dups, stores all nodes in hashtable [nodes],
       distinguishing the closed list by a special q_pos. note 3 kinds of nodes:
       1) in openlist, garbage q_pos (init_pos or positive int)
       2) in openlist, valid q_pos (positive int)
       3) not in openlist, q_pos = closed_pos
    *)
  let openlist = Geq.create_with
    ~equals:(fun a b ->
	       sface.Search_interface.equals
		 (sface.Search_interface.key a)
		 (sface.Search_interface.key b))
    ordered1 ordered2 close_enough setpos getpos sface.Search_interface.initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 250
  and i = sface.Search_interface.info in

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
		  then (Geq.remove openlist entry;
			setpos prev closed_pos;
			Htable.replace nodes state
			  (Geq.insert openlist n))
		    (* The entry is not somewhere in the geq, we need to
		       re-insert it *)
		  else (Htable.replace nodes state
			  (Geq.insert openlist n)))
	 with Not_found ->
	   Htable.add nodes state (Geq.insert openlist n)) in

  let rec do_loop () =
    if ((not (Geq.empty_p openlist)) && (not (Limit.halt_p i)))
    then
      (record i openlist;
       let n = Geq.peek_best openlist in
	 Geq.remove_best openlist;
	 setpos n closed_pos;
	 if (not (Limit.promising_p i n))
	 then Limit.incr_prune i
	 else (if sface.Search_interface.goal_p n
	       then Limit.new_incumbent i (Limit.Incumbent (0.,n))
	       else
		 (Limit.incr_exp i;
		  List.iter insert_child
		    (sface.Search_interface.node_expand n);
		  do_loop ())))
  in
    Htable.add nodes
      (sface.Search_interface.key sface.Search_interface.initial)
      (Geq.insert openlist sface.Search_interface.initial);
    (*Verb.pe Verb.toplvl "Initial Child Added.  Starting Search\n";*)
    do_loop ();
    Limit.results6 i


let search_dups_at_expand ?(record = no_record) sface ordered1 close_enough
    ordered2 better_p setpos getpos =
  (* special q_pos values *)
  (** take [key] as additional argument *)
  (* like a_star_dups, stores all nodes in hashtable [nodes],
     distinguishing the closed list by a special q_pos. note 3 kinds of nodes:
     1) in openlist, garbage q_pos (init_pos or positive int)
     2) in openlist, valid q_pos (positive int)
     3) not in openlist, q_pos = closed_pos
  *)
  let openlist = Geq.create_with ordered1 ordered2 close_enough
    setpos getpos sface.Search_interface.initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 250
  and i = sface.Search_interface.info in
  let check_child n =
    (** return false iff [n] can be discarded *)
    Limit.incr_gen i;
    if not (Limit.promising_p i n) then
      (Limit.incr_prune i;
       false)
    else
      let state = sface.Search_interface.key n in
	try
	  let prev = Htable.find nodes state in
	    Limit.incr_dups i;
	    not (better_p prev n) (*prev.f > n.f*)
	with Not_found ->
	  true in

  let geq_expand n =
    (** specifies return value of false iff found goal *)
    if not (Limit.promising_p i n)
    then (Limit.incr_prune i;
	  [], true)
    else
      (Htable.replace nodes (sface.Search_interface.key n) n;
       if sface.Search_interface.goal_p n
       then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	     [], false)
       else
	 (let children = sface.Search_interface.node_expand n in
	    (Limit.incr_exp i;
	     (List.filter check_child children), true)))

  and geq_record n entry =
    ()
  and continue = ref true in

  let do_loop () =
    while (not (Geq.empty_p openlist)) && (not (Limit.halt_p i)) && !continue
    do
      (record i openlist;
       let children,cont = geq_expand (Geq.peek_best openlist) in
	 continue := cont;
	 if !continue
	 then
	   ((*Verb.pe Verb.toplvl "Removing best node\n";*)
	    Geq.remove_best openlist;
	    (*Verb.pe Verb.toplvl "Inserting Children\n";*)
	    List.iter
	      (fun c -> geq_record c (Geq.insert openlist c)) children;
	    (*Verb.pe Verb.toplvl "Counting Geq\n";*)
	    Limit.curr_q i (Geq.count openlist);
	    (*Verb.pe Verb.toplvl "Recuring\n"*)))
    done
  in

    ignore (Geq.insert openlist sface.Search_interface.initial);
    (*Verb.pe Verb.toplvl "Initial Child Added.  Starting Search\n";*)
    do_loop ();
    Limit.results6 i


let search_dd ?(record = no_record) sface ordered1 close_enough
    ordered2 better_p setpos getpos =
  (* special q_pos values *)
  let closed_pos = -2 in
    (** take [key] as additional argument *)
    (* like a_star_dups, stores all nodes in hashtable [nodes],
       distinguishing the closed list by a special q_pos. note 3 kinds of nodes:
       1) in openlist, garbage q_pos (init_pos or positive int)
       2) in openlist, valid q_pos (positive int)
       3) not in openlist, q_pos = closed_pos
    *)
  let openlist = Geq.create_with
    ~equals:(fun a b ->
	       sface.Search_interface.equals
		 (sface.Search_interface.key a)
		 (sface.Search_interface.key b))
    ordered1 ordered2 close_enough setpos getpos sface.Search_interface.initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 250
  and i = sface.Search_interface.info in

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
		  then (Geq.remove openlist entry;
			setpos prev closed_pos;
			Htable.replace nodes state
			  (Geq.insert openlist n)))
	 with Not_found ->
	   Htable.add nodes state (Geq.insert openlist n)) in

  let rec do_loop () =
    if ((not (Geq.empty_p openlist)) && (not (Limit.halt_p i)))
    then
      (record i openlist;
       let n = Geq.peek_best openlist in
	 Geq.remove_best openlist;
	 setpos n closed_pos;
	 if (not (Limit.promising_p i n))
	 then Limit.incr_prune i
	 else (if sface.Search_interface.goal_p n
	       then Limit.new_incumbent i (Limit.Incumbent (0.,n))
	       else
		 (Limit.incr_exp i;
		  List.iter insert_child
		    (sface.Search_interface.node_expand n);
		  do_loop ())))
  in
    Htable.add nodes
      (sface.Search_interface.key sface.Search_interface.initial)
      (Geq.insert openlist sface.Search_interface.initial);
    (*Verb.pe Verb.toplvl "Initial Child Added.  Starting Search\n";*)
    do_loop ();
    Limit.results6 i


let search_dups_at_expand ?(record = no_record) sface ordered1 close_enough
    ordered2 better_p setpos getpos =
  (* special q_pos values *)
  (** take [key] as additional argument *)
  (* like a_star_dups, stores all nodes in hashtable [nodes],
     distinguishing the closed list by a special q_pos. note 3 kinds of nodes:
     1) in openlist, garbage q_pos (init_pos or positive int)
     2) in openlist, valid q_pos (positive int)
     3) not in openlist, q_pos = closed_pos
  *)
  let openlist = Geq.create_with ordered1 ordered2 close_enough
    setpos getpos sface.Search_interface.initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 250
  and i = sface.Search_interface.info in
  let check_child n =
    (** return false iff [n] can be discarded *)
    Limit.incr_gen i;
    if not (Limit.promising_p i n) then
      (Limit.incr_prune i;
       false)
    else
      let state = sface.Search_interface.key n in
	try
	  let prev = Htable.find nodes state in
	    Limit.incr_dups i;
	    not (better_p prev n) (*prev.f > n.f*)
	with Not_found ->
	  true in

  let geq_expand n =
    (** specifies return value of false iff found goal *)
    if not (Limit.promising_p i n)
    then (Limit.incr_prune i;
	  [], true)
    else
      (Htable.replace nodes (sface.Search_interface.key n) n;
       if sface.Search_interface.goal_p n
       then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	     [], false)
       else
	 (let children = sface.Search_interface.node_expand n in
	    (Limit.incr_exp i;
	     (List.filter check_child children), true)))

  and geq_record n entry =
    ()
  and continue = ref true in

  let do_loop () =
    while (not (Geq.empty_p openlist)) && (not (Limit.halt_p i)) && !continue
    do
      (record i openlist;
       let children,cont = geq_expand (Geq.peek_best openlist) in
	 continue := cont;
	 if !continue
	 then
	   ((*Verb.pe Verb.toplvl "Removing best node\n";*)
	    Geq.remove_best openlist;
	    (*Verb.pe Verb.toplvl "Inserting Children\n";*)
	    List.iter
	      (fun c -> geq_record c (Geq.insert openlist c)) children;
	    (*Verb.pe Verb.toplvl "Counting Geq\n";*)
	    Limit.curr_q i (Geq.count openlist);
	    (*Verb.pe Verb.toplvl "Recuring\n"*)))
    done
  in

    ignore (Geq.insert openlist sface.Search_interface.initial);
    (*Verb.pe Verb.toplvl "Initial Child Added.  Starting Search\n";*)
    do_loop ();
    Limit.results6 i

(* EOF *)

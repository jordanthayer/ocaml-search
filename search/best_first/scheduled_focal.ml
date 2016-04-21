(** Very similar to the focal code accepts an additional function
    which serves the nodes from one queue or the other.
    Jordan - July 2009
*)

let no_record = (fun _ _ -> ())


let no_dups ?(record = no_record) sface f_order focal_order close_enough
    better_p setpos getpos get_node =
  let openlist = Geq.create_with f_order focal_order close_enough
    setpos getpos sface.Search_interface.initial
  and i = sface.Search_interface.info in
    ignore (Geq.insert openlist sface.Search_interface.initial);
    let check_child n =
      Limit.incr_gen i;
      if Limit.promising_p i n
      then true
      else (Limit.incr_prune i;
	    false) in

    let geq_expand n =
      (** specify false return value iff found goal *)
      if not (Limit.promising_p i n) then
	(Limit.incr_prune i;
	 [], true)
      else if sface.Search_interface.goal_p n then
	(Limit.new_incumbent i (Limit.Incumbent (0., n));
	 [], false)
      else
	(Limit.incr_exp i;
	 (List.filter check_child
	    (sface.Search_interface.node_expand n), true)) in

    let rec do_loop () =
      if (not (Geq.empty_p openlist)) && (not (Limit.halt_p i))
      then
	(record i openlist;
	 let n = get_node openlist in
	 let kids, cont = geq_expand n in
	   if cont
	   then
	     (List.iter (fun c -> ignore (Geq.insert openlist c)) kids;
	      Limit.curr_q i (Geq.count openlist);
	      do_loop()))
    in
      do_loop ();
      Limit.results5 i


let dups ?(record = no_record) sface f_order focal_order close_enough better_p
    setpos getpos get_node =
  let openlist = Geq.create_with f_order focal_order close_enough
    setpos getpos sface.Search_interface.initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 250
  and i = sface.Search_interface.info in

  let add_node n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then (Limit.incr_prune i;
	  Htable.remove nodes (sface.Search_interface.key n))
    else
      (let state = sface.Search_interface.key n in
	 try
	   let entry = Htable.find nodes state in
	     Limit.incr_dups i;
	     let prev = Geq.data entry in
	       if not (better_p prev n)
	       then (if (getpos prev) <> Dpq.no_position
		     then Geq.remove openlist entry;
		     Htable.replace nodes (sface.Search_interface.key n)
		       (Geq.insert openlist n))
	 with Not_found ->
	   Htable.replace nodes (sface.Search_interface.key n)
	     (Geq.insert openlist n)) in

  let geq_expand n =
    let hashed = Htable.find nodes (sface.Search_interface.key n) in
      Geq.remove openlist hashed;
      setpos n Dpq.no_position;
      if not (Limit.promising_p i n)
      then
	(Limit.incr_prune i;
	 Htable.remove nodes (sface.Search_interface.key n);
	 [],true)
      else
	(if sface.Search_interface.goal_p n
	 then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	       (*Verb.pe Verb.toplvl "new goal\n%!";*)
	       [],false)
	 else
	   (Limit.incr_exp i;
	    sface.Search_interface.node_expand n,true)) in

  let rec do_loop () =
    if not (Geq.empty_p openlist) && not (Limit.halt_p i)
    then
      (record i openlist;
       let n = get_node openlist in
       let children, cont = geq_expand n in
	 if cont
	 then (List.iter add_node children;
	       Limit.curr_q i (Geq.count openlist);
	       do_loop()))
  in
    Htable.replace nodes (sface.Search_interface.key
			     sface.Search_interface.initial)
      (Geq.insert openlist sface.Search_interface.initial);
    do_loop ();
    Limit.results6 i


(* EOF *)

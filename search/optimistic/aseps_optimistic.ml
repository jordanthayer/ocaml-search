(** Optimistic search which uses A* epsilon in the first phase
    Jordan - July 2009 *)

let no_record = (fun _ _ _ -> ())
let closed_pos = -2


let search ?(record = no_record) sface ordered1 ordered2 close_p better_p
    setpos getpos setcp getcp bound get_node =
  (** Optimistic search using A* epsilon as the optimistic portion of the
      search.  [Sface] is the search interface.  [Ordered1] orders focal while
      [ordered2] orders open.  [close_p] limits when you make it on to focal.
      [better_p] determines new incumbents.  [setpos] and [getpos] are utility
      functions used by the good enough queue. [setcp] and [getcp] maintain
      position on the closed list.  [bound] is the desicred quaity bound and
      [get_node] fetches the next node to be expanded *)
  let openlist = Geq.create_with ordered1 ordered2 close_p
    setpos getpos sface.Search_interface.initial
  and cleanup = Dpq.create better_p setcp 100 sface.Search_interface.initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 250
  and i = sface.Search_interface.info in

  let add_node n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else
      (let state = sface.Search_interface.key n in
	 try
	   let entry = Htable.find nodes state in
	     Limit.incr_dups i;
	     let prev = Geq.data entry in
	       if not (better_p prev n)
	       then (if (getcp prev) >= 0
		     then (Geq.remove openlist entry;
			   Dpq.remove cleanup (getcp prev));
		     Dpq.insert cleanup n;
		     Htable.replace nodes (sface.Search_interface.key n)
		       (Geq.insert openlist n))
	 with Not_found ->
	   Dpq.insert cleanup n;
	   Htable.replace nodes (sface.Search_interface.key n)
	     (Geq.insert openlist n)) in

  let geq_expand n =
    Limit.incr_exp i;
    let hashed = Htable.find nodes (sface.Search_interface.key n) in
      Geq.remove openlist hashed;
      Dpq.remove cleanup (getcp n);
      setpos n closed_pos;
      setcp n closed_pos;
      if not (Limit.promising_p i n)
      then
	(Limit.incr_prune i;
	 [])
      else
	(if sface.Search_interface.goal_p n
	 then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	       [])
	 else sface.Search_interface.node_expand n) in

  let rec do_loop () =
    if not (Geq.empty_p openlist) && not (Limit.halt_p i)
    then
      (record i openlist cleanup;
       let n,recur = get_node cleanup openlist i in
	 if recur
	 then (let children = geq_expand n in
		 List.iter add_node children;
		 Limit.curr_q i (Geq.count openlist);
		 do_loop())) in
    Dpq.insert cleanup sface.Search_interface.initial;
    Htable.add nodes (sface.Search_interface.key
			 sface.Search_interface.initial)
      (Geq.insert openlist sface.Search_interface.initial);
    do_loop ();
    i


(**************** Searches *********************************)

let no_dups sface ordered1 ordered2 close_p better_p setpos getpos setcp getcp
    bound get_node =
  (** Aseps optimistic search for domains with few / no duplicates*)
  Limit.results5
    (search sface ordered1 ordered2 close_p better_p
       setpos getpos setcp getcp bound get_node)

and dups sface ordered1 ordered2 close_p better_p setpos getpos setcp getcp
    bound get_node =
  (** Aseps optimistic search for domains with many duplicates. *)
  Limit.results6
    (search sface ordered1 ordered2 close_p better_p
       setpos getpos setcp getcp bound get_node)

(* EOF *)

(** Anytime Focal List Based Search Algorithms
    Jordan - Jan 2010 *)

let no_record = (fun _ _ -> ())

let search ?(record = no_record) sface ordered1 close_enough ordered2 better_p
    setpos getpos =
  let openlist = Geq.create_with ordered1 ordered2 close_enough
    setpos getpos sface.Search_interface.initial
  and i = sface.Search_interface.info in
  let check_child n =
    Limit.incr_gen i;
    if Limit.promising_p i n
    then true
    else (Limit.incr_prune i; false) in
  let geq_expand n =
    (** specify false return value iff found goal *)
    if not (Limit.promising_p i n)
    then (Limit.incr_prune i; [], true)
    else if sface.Search_interface.goal_p n
    then (Limit.new_incumbent i (Limit.Incumbent (0., n)); [],true)
    else
      (let children = sface.Search_interface.node_expand n in
	 (Limit.incr_exp i;
	  (List.filter check_child children),true)) in
  let rec do_loop () =
    if (not (Geq.empty_p openlist)) && (not (Limit.halt_p i))
    then
      (record i openlist;
       ignore (Geq.replace_using geq_expand Fn.no_op2 openlist);
       (Limit.curr_q i (Geq.count openlist);
	do_loop ()))
  in
    ignore (Geq.insert openlist sface.Search_interface.initial);
    do_loop ();
    Limit.results5 i


let search_dups ?(record = no_record) sface ordered1 close_enough
    ordered2 better_p setpos getpos =
  let closed_pos = -2 in
    (** take [key] as additional argument *)
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
		 (if (getpos prev) <> closed_pos
		  then (Geq.remove openlist entry;
			setpos prev closed_pos;
			Htable.replace nodes state
			  (Geq.insert openlist n))
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
		    (sface.Search_interface.node_expand n)));
	 do_loop ())
  in
    Htable.add nodes
      (sface.Search_interface.key sface.Search_interface.initial)
      (Geq.insert openlist sface.Search_interface.initial);
    do_loop ();
    Limit.results6 i


(* EOF *)

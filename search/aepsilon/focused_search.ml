(** Engines for doing A* epsilon *)

let no_dups ?(anytime = false) persevere
    sface ordered_f ordered_focal close_enough better_p setpos getpos =
  (** A very simple version of Aepsilon with no persevere predicate.
      Performance should be very very similar to that of A* epsilon *)
  let openlist = (Geq.create_with ordered_f ordered_focal close_enough
		    setpos getpos sface.Search_interface.initial)
  and i = sface.Search_interface.info
  and expand = sface.Search_interface.node_expand in
  let insert n = Limit.incr_gen i; ignore (Geq.insert openlist n) in

  let rec do_expand min_f n =
    (* Follows a single search path until it dead-ends or becomes too costly*)
    if sface.Search_interface.goal_p n
    then Limit.new_incumbent i (Limit.Incumbent (0., n))
    else
      (if not (Limit.promising_p i n) || (Limit.halt_p i)
       then () (* node isn't promising or time is up *)
       else
	 (Limit.incr_exp i;
	  (if Geq.empty_p openlist
	   then (match expand n with
		     [] -> ()
		   | hd::tl ->
		       (let best =
			  (List.fold_left
			     (fun accum ele ->
				(if ordered_focal accum ele
				 then accum else ele)) hd tl) in
			let others = (List.filter (fun n -> n <> best)
					(hd::tl)) in
			  List.iter insert others;
			  do_expand (Geq.peek_doset openlist) best))
	   else
	     (match expand n with
		| [] -> ()
		| kids ->
		    (match List.filter (fun n -> close_enough n min_f) kids
		     with [] ->
		       (let best =
			  (List.fold_left
			     (fun accum ele ->
				(if ordered_focal accum ele
				 then accum else ele)) (List.hd kids)
			     (List.tl kids)) in
			  if persevere min_f n
			  then (if min_f <> n
				then (List.iter insert
					(expand (Geq.remove_doset openlist)));
				List.iter insert
				  (List.filter
				     (fun n -> n != best) kids);
				do_expand (Geq.peek_doset openlist) best)
			  else List.iter insert kids)
		       | hd::tl ->
			   (let best =
			      (List.fold_left
				 (fun accum ele ->
				    (if ordered_focal accum ele
				     then accum else ele)) hd tl) in
			    let others = (List.filter (fun n -> n != best)
					    kids) in
			      List.iter insert others;
			      do_expand (Geq.peek_doset openlist) best)))))) in

  let rec do_loop () =
    if not (Geq.empty_p openlist) && (not (Limit.halt_p i)) &&
      (anytime || (match i.Limit.incumbent with Limit.Nothing -> true
		     | Limit.Incumbent (q,n) ->
			 not (close_enough (Geq.peek_doset openlist) n)))
    then
      (let n = Geq.remove_best openlist in
	 do_expand (Geq.peek_doset openlist) n;
	 do_loop()) in

    insert sface.Search_interface.initial;
    do_loop();
    Limit.results5 i



let dups ?(anytime = false) persevere sface ordered_f ordered_focal
    close_enough better_p setpos getpos =
  let openlist = (Geq.create_with ordered_f ordered_focal close_enough
		    setpos getpos sface.Search_interface.initial)
  and closed = (Htable.create sface.Search_interface.hash
		  sface.Search_interface.equals 100)
  and i = sface.Search_interface.info
  and expand = sface.Search_interface.node_expand
  and key = sface.Search_interface.key
  and closed_pos = Dpq.no_position in

  let insert n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else (let state = key n in
	    try (let entry = Htable.find closed state in
		 let prev = Geq.data entry in
		   Limit.incr_dups i;
		   if not (better_p prev n)
		   then (if (getpos prev) <> closed_pos
			 then (Geq.remove openlist entry;
			       setpos prev closed_pos;
			       Htable.replace closed state
				 (Geq.insert openlist n))
			 else (Htable.replace closed state
				 (Geq.insert openlist n))))
	    with Not_found ->
	      Htable.add closed state (Geq.insert openlist n)) in

  let rec min_f_step next =
    if Limit.halt_p i then false
    else
      (let to_exp = Geq.remove_doset openlist in
	 setpos to_exp closed_pos;
	 if sface.Search_interface.goal_p to_exp
	 then (Limit.new_incumbent i (Limit.Incumbent (0.,to_exp));
	       false)
	 else
	   (Limit.incr_exp i;
	    List.iter insert (expand to_exp);
	    if not (Geq.empty_p openlist)
	    then (let min = Geq.peek_doset openlist in
		    if close_enough min next
		    then true
		    else (if persevere min next
			  then min_f_step next
			  else false))
	    else true)) in

  let rec do_expand n =
    setpos n closed_pos;
    if not (Geq.empty_p openlist)
    then (assert (close_enough (Geq.peek_doset openlist) n));
    if sface.Search_interface.goal_p n
    then Limit.new_incumbent i (Limit.Incumbent (0.,n))
    else
      (if (not (Limit.promising_p i n)) || (Limit.halt_p i)
       then ()
       else
	 (Limit.incr_exp i;
	  match expand n with
	      [] -> () (* n dead ends, start over *)
	    | hd :: tl ->
		(let best =
		   (List.fold_left
		      (fun accum ele ->
			 (if ordered_focal accum ele
			  then accum else ele)) hd tl) in
		 let others = List.filter (fun n -> n != best) (hd::tl) in
		   List.iter insert others;
		   if Geq.empty_p openlist
		   then do_expand best (* openlist is empty, best must be
					  admissible *)
		   else
		     (let f_min = Geq.peek_doset openlist in
			if close_enough f_min best
			then do_expand best
			else (if persevere f_min best
			      then (if min_f_step best
				    then do_expand best
				    else insert best)
			      else insert best)))))
  in

  let rec do_loop () =
    if not (Geq.empty_p openlist) && (not (Limit.halt_p i)) &&
      (anytime || (match i.Limit.incumbent with Limit.Nothing -> true
		     | Limit.Incumbent (q,n) ->
			 not (close_enough (Geq.peek_doset openlist) n)))
    then
      (let n = Geq.remove_best openlist in
	 do_expand n;
	 do_loop()) in

    insert sface.Search_interface.initial;
    do_loop();
    Limit.results6 i


(* EOF *)

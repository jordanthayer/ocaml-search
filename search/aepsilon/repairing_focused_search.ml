(** Engines for doing ARA epsilon -- These should be doing something more
    intelligent with the inconsistent lists than they are currently. *)

let no_dups wtlist persevere sface ordered_f ordered_focal make_close_enough
    better_p setpos getpos =
  (** A very simple version of Aepsilon with no persevere predicate.
      Performance should be very very similar to that of A* epsilon *)
  let wts = ref wtlist in
  let openlist = (Geq.create_with ordered_f ordered_focal
		    (make_close_enough (List.hd wtlist))
		    setpos getpos sface.Search_interface.initial)
  and i = sface.Search_interface.info
  and expand = sface.Search_interface.node_expand in
  let insert n = Limit.incr_gen i; ignore (Geq.insert openlist n) in

  let rec do_expand min_f n =
    (* Follows a single search path until it dead-ends or becomes too costly*)
    if sface.Search_interface.goal_p n
    then (Limit.new_incumbent i (Limit.Incumbent (0., n));
	  wts := List.tl !wts;
	  if (List.length !wts) > 0
	  then Geq.update_close_enough openlist
	    (make_close_enough (List.hd !wts)))
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
		    (match List.filter (fun n ->
					  make_close_enough (List.hd !wts)
					     n min_f) kids
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
      (List.length !wts > 0)
    then (let n = Geq.remove_best openlist in
	    do_expand (Geq.peek_doset openlist) n;
	    do_loop()) in
    insert sface.Search_interface.initial;
    do_loop();
    Limit.results5 i


let dups wtlist persevere sface ordered_f ordered_focal make_close_enough
    better_p setpos getpos =
  let wts = ref wtlist in
  let openlist = (Geq.create_with ordered_f ordered_focal
		    (make_close_enough (List.hd wtlist))
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

  let rec do_expand min_f n =
    if sface.Search_interface.goal_p n
    then (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	  wts := List.tl !wts;
	  if (List.length !wts) > 0
	  then Geq.update_close_enough openlist
	    (make_close_enough (List.hd !wts)))
    else
      (if (not (Limit.promising_p i n)) || (Limit.halt_p i)
       then ()
       else
	 (Limit.incr_exp i;
	  if Geq.empty_p openlist
	  then (match expand n with
		    [] -> ()
		  | hd::tl ->
		      (let best =
			 (List.fold_left
			    (fun accum ele ->
			       (if ordered_focal accum ele
				then accum else ele)) hd tl) in
		       let others = (List.filter (fun n -> n != best)
				       (hd::tl)) in
			 List.iter insert others;
			 do_expand (Geq.peek_doset openlist) best))
	  else
	    (match List.filter
	       (fun n ->
		  Limit.incr_gen i;
		  if not (Htable.mem closed (key n))
		  then (Htable.add closed (key n) (Geq.insert openlist n); true)
		  else
		    (let prev = Htable.find closed (key n) in
		       if better_p (Geq.data prev) n
		       then false
		       else (Htable.replace closed (key n)
			       (Geq.insert openlist n);
			     true))) (expand n) with
		 | [] -> ()
		 | kids ->
		     (match List.filter (fun n ->
					   make_close_enough (List.hd !wts)
					     n min_f) kids with
			| [] ->
			    (let best =
			       (List.fold_left
				  (fun accum ele ->
				     (if ordered_focal accum ele
				      then accum else ele)) (List.hd kids)
				  (List.tl kids)) in
			       if persevere min_f best
			       then (List.iter insert
				       (expand (Geq.remove_doset openlist));
				     do_expand (Geq.peek_doset openlist) best))
			| hd::tl ->
			    (let best =
			       (List.fold_left
				  (fun accum ele ->
				     (if ordered_focal accum ele
				      then accum else ele)) hd tl) in
			       do_expand (Geq.peek_doset openlist) best)))))
  in

  let rec do_loop () =
    if not (Geq.empty_p openlist) && (not (Limit.halt_p i)) &&
      ((List.length !wts) > 0)
    then
      (let n = Geq.remove_best openlist in
	 do_expand (Geq.peek_doset openlist) n;
	 do_loop()) in

    insert sface.Search_interface.initial;
    do_loop();
    Limit.results6 i


(* EOF *)

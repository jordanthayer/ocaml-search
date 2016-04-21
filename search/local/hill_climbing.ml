(** Hill Climbing search framework - Jordan Feb 2010 *)

type 'a kids =
  | BestChild of 'a
  | Goal
  | NoImprovement


let proc_child goal i better_p =
  (fun current accum child ->
     Limit.incr_gen i;
     match accum with
       | BestChild best -> (if goal child
			    then (Limit.new_incumbent i
				    (Limit.Incumbent (0., child));
				  Goal)
			    else (if better_p child best
				  then BestChild child
				  else accum))
       | Goal -> (if goal child then Limit.new_incumbent i
		    (Limit.Incumbent (0., child));
		  accum)
       | NoImprovement -> (if goal child
			   then (Limit.new_incumbent i
				   (Limit.Incumbent (0., child));
				 Goal)
			   else (if better_p child current
				 then BestChild child
				 else accum)))


let search sface better_p =
  (* Standard vanilla hillclimbing *)
  let goal = sface.Search_interface.goal_p
  and i = sface.Search_interface.info
  and expand = sface.Search_interface.node_expand in
  let process_child = proc_child goal i better_p in

  let rec do_step current =
    if not (Limit.halt_p i)
    then
      (Limit.incr_exp i;
       match (List.fold_left
		(process_child current) NoImprovement (expand current)) with
	   BestChild next -> do_step next
	 | _ -> i)
    else i in
    do_step sface.Search_interface.initial


let enforced_search sface better_p =
  let goal = sface.Search_interface.goal_p
  and i = sface.Search_interface.info
  and expand = sface.Search_interface.node_expand
  and closed = (Htable.create sface.Search_interface.hash
		  sface.Search_interface.equals 100) in
  let process_child = proc_child goal i better_p in

  let rec append list1 list2 =
    match list2 with
	[] -> list1
      | hd::tl -> append (hd::list1) tl in

  let rec do_step (last_best, current_list) =
    if not (Limit.halt_p i)
    then (let children =
	    List.fold_left
	      (fun accum element ->
		 append accum element)
	      []
	      (List.fold_left
		 (fun accum ele ->
		    if not (Limit.halt_p i)
		    then
		      (Limit.incr_exp i;
		       (List.filter
			  (fun element ->
			     let state = sface.Search_interface.key element
			     in
			       if Htable.mem closed state
			       then false
			       else (Htable.add closed state ele;
				     true)) (expand ele))::accum)
		    else accum) []
		 current_list) in
	    if not (Limit.halt_p i)
	    then
	      (match (List.fold_left (process_child last_best)
			NoImprovement children) with
		   BestChild next -> (Htable.clear closed;
				      do_step (next,[next]))
		 | NoImprovement -> do_step (last_best, children)
		 | Goal -> i)
	    else i)
    else i in
    do_step (sface.Search_interface.initial,[sface.Search_interface.initial])


let next_best_search sface better_p =
  let goal = sface.Search_interface.goal_p
  and i = sface.Search_interface.info
  and expand = sface.Search_interface.node_expand in
  let process_child = proc_child goal i better_p in

  let rec do_step current =
    if not (Limit.halt_p i)
    then
      (let kids = expand current in
	 Limit.incr_exp i;
	 match (List.fold_left
		  (process_child current) (BestChild (List.hd kids)) kids) with
	     BestChild next -> do_step next
	   | _ -> i)
    else i in
    do_step sface.Search_interface.initial


let random_walk_search sface better_p =
  let goal = sface.Search_interface.goal_p
  and i = sface.Search_interface.info
  and expand = sface.Search_interface.node_expand in
  let process_child = proc_child goal i better_p in

  let rec do_step current =
    if not (Limit.halt_p i)
    then
      (let kids = expand current in
	 Limit.incr_exp i;
	 match (List.fold_left
		  (process_child current) NoImprovement kids) with
	   | BestChild next -> do_step next
	   | NoImprovement -> do_step (Wrlist.random_elt kids)
	   | Goal -> i)
    else i in
    do_step sface.Search_interface.initial

(* EOF *)

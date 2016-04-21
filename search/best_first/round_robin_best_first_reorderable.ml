(**

    @author jtd7
    @since 2010-09-29

   Perform best first search on multiple queues
*)

let init_lists sface num_queues preds setpos =
  (** constructs the lists to be used during search *)
  let initial = sface.Search_interface.initial in
  let nodes = (Htable.create sface.Search_interface.hash
		 sface.Search_interface.equals 100)
  and openlists = (Array.init num_queues
		     (fun i -> Dpq.create (preds i) (setpos i) 100 initial)) in
    openlists, nodes


let search sface num_queues ordered_ps set_pos get_pos better_p update_open =
  let cur_queue = ref 0 in
  let incr_queue () = (let cq =  !cur_queue in
			 if cq = (num_queues - 1)
			 then cur_queue := 0
			 else cur_queue := cq + 1)
  and openlists, _ = init_lists sface num_queues  ordered_ps set_pos
  and i = sface.Search_interface.info in
  let insert n = (for i = 0 to (num_queues - 1) do
		    Dpq.insert openlists.(i) n
		  done)
  and empty_p () = Dpq.empty_p openlists.(0)
  and extract () = (let cq = !cur_queue in
		    let n = Dpq.extract_first openlists.(cq) in
		      incr_queue();
		      for i = 0 to num_queues - 1 do
			if cq <> i
			then Dpq.remove openlists.(i) (get_pos i n)
		      done;
		      n) in
  let rec expand_best () =
    if (not (empty_p())) && (not (Limit.halt_p i))
    then (let n = extract () in
	    if not (Limit.promising_p i n)
	    then (Limit.incr_prune i;
		  expand_best())
	    else (if sface.Search_interface.goal_p n
		  then Limit.new_incumbent i (Limit.Incumbent (0., n))
		  else (Limit.incr_exp i;
			let reorder, kids =
			  sface.Search_interface.resort_expand n in
			List.iter (fun c ->
				     Limit.incr_gen i;
				     if Limit.promising_p i c
				     then insert c
				     else Limit.incr_prune i) kids;
			Limit.curr_q i (Dpq.count (openlists.(0)));
			expand_best ()))) in
    insert sface.Search_interface.initial;
    expand_best ();
    Limit.results5 i


let search_dups sface num_queues ordered_ps set_pos get_pos better_p
    update_open =
  let cur_queue = ref 0 in
  let incr_queue () = (let cq =  !cur_queue in
			 if cq = (num_queues - 1)
			 then cur_queue := 0
			 else cur_queue := cq + 1)
  and openlists, closed = init_lists sface num_queues ordered_ps set_pos
  and i = sface.Search_interface.info in
  let insert n = (let state = sface.Search_interface.key n in
		    try
		      let prev = Htable.find closed state in
			Limit.incr_dups i;
			if not (better_p prev n)
			then
			  (Htable.replace closed state n;
			   for i = 0 to (num_queues - 1) do
			     (let pos = get_pos i prev in
				if pos = Dpq.no_position
				then Dpq.insert openlists.(i) n
				else Dpq.swap openlists.(i) pos n)
			   done)
		    with Not_found ->
		      (Htable.add closed state n;
		       for i = 0 to (num_queues - 1) do
			 Dpq.insert openlists.(i) n
		       done))
  and empty_p () = Dpq.empty_p openlists.(0)
  and extract () = (let cq = !cur_queue in
		    let n = Dpq.extract_first openlists.(cq) in
		      incr_queue();
		      for i = 0 to (num_queues - 1) do
			(if cq <> i
			 then Dpq.remove openlists.(i) (get_pos i n);
			 set_pos i n Dpq.no_position)
		      done;
		      n) in
  let rec expand_best () =
    if (not (empty_p())) && (not (Limit.halt_p i))
    then (let n = extract () in
	    if not (Limit.promising_p i n)
	    then (Limit.incr_prune i;
		  expand_best())
	    else (if sface.Search_interface.goal_p n
		  then Limit.new_incumbent i (Limit.Incumbent (0., n))
		  else (Limit.incr_exp i;
			let reorder, kids =
			  sface.Search_interface.resort_expand n in
			List.iter (fun c ->
				     Limit.incr_gen i;
				     if Limit.promising_p i c
				     then insert c
				     else Limit.incr_prune i) kids;
			Limit.curr_q i (Dpq.count (openlists.(0)));
			expand_best ()))) in
    insert sface.Search_interface.initial;
    expand_best ();
    Limit.results6 i



let search_dd sface num_queues ordered_ps set_pos get_pos better_p update_open =
  let cur_queue = ref 0 in
  let incr_queue () = (let cq =  !cur_queue in
			 if cq = (num_queues - 1)
			 then cur_queue := 0
			 else cur_queue := cq + 1)
  and openlists, closed = init_lists sface num_queues ordered_ps set_pos
  and i = sface.Search_interface.info in
  let insert n = (let state = sface.Search_interface.key n in
		    if not (Htable.mem closed state)
		    then
		      (Htable.add closed state n;
		       for i = 0 to (num_queues - 1) do
			 Dpq.insert openlists.(i) n
		       done))
  and empty_p () = Dpq.empty_p openlists.(0)
  and extract () = (let cq = !cur_queue in
		    let n = Dpq.extract_first openlists.(cq) in
		      incr_queue();
		      for i = 0 to (num_queues - 1) do
			(if cq <> i
			 then Dpq.remove openlists.(i) (get_pos i n);
			 set_pos i n Dpq.no_position)
		      done;
		      n) in
  let rec expand_best () =
    if (not (empty_p())) && (not (Limit.halt_p i))
    then (let n = extract () in
	    if not (Limit.promising_p i n)
	    then (Limit.incr_prune i;
		  expand_best())
	    else (if sface.Search_interface.goal_p n
		  then Limit.new_incumbent i (Limit.Incumbent (0., n))
		  else (Limit.incr_exp i;
			let reorder, kids =
			  sface.Search_interface.resort_expand n in
			List.iter (fun c ->
				     Limit.incr_gen i;
				     if Limit.promising_p i c
				     then insert c
				     else Limit.incr_prune i) kids;
			Limit.curr_q i (Dpq.count (openlists.(0)));
			expand_best ()))) in
    insert sface.Search_interface.initial;
    expand_best ();
    Limit.results6 i


(* EOF *)

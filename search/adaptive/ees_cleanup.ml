(**

    @author jtd7
    @since 2011-01-11

   Explicit estimation search without the conservative bent, does an
   A* epsilon search, then cleans up.
*)

open Three_queue_search_ss_v2

let make_expand expand hd calc_hd_data fd_calc =
  let init = Geq.make_dummy_entry ()
  and update = make_update fd_calc in
    (fun n ->
       let best_f = ref infinity
       and best_child = ref n
       and nd = n.depth + 1 in
       let children = (List.map (fun (s, g) ->
				   let h, d = hd s in
				   let f = g +. h in
				   let h_err = f -. n.f +. n.h_err
				   and d_err = d -. n.d +. 1. +. n.d_err in
				   let h_err = (if Math.finite_p h_err
						then h_err else n.h_err)
				   and d_err = (if Math.finite_p d_err
						then d_err else n.d_err) in
				   let c = (make_child s g nd h d 0. 0.
					      init h_err d_err f) in
				     if  f < !best_f then
				       (best_child := c;
					best_f := f)
				     else if f = !best_f then
				       (if d < !best_child.d then
					  (best_child := c;
					   best_f := f));
				     c)
			 (expand n.data n.g))
       in
	 if not ((List.length children) = 0)
	 then
	   (calc_hd_data n !best_child children;
	    List.iter update children);
	 children)


let make_expand_pathmax expand hd calc_hd_data fd_calc =
  let init = Geq.make_dummy_entry()
  and update = make_update fd_calc in
    (fun n ->
       let best_f = ref infinity
       and best_child = ref n
       and nd = n.depth + 1 in
       let children = (List.map (fun (s, g) ->
				   let hval, d = hd s
				   and t_cost = g -. n.g in
				   let h = Math.fmax hval (n.h -. t_cost) in
				   let f = g +. h in
				   let h_err = f -. n.f +. n.h_err
				   and d_err = d -. n.d +. 1. +. n.d_err in
				   let h_err = (if Math.finite_p h_err
						then h_err
						else n.h_err)
				   and d_err = (if Math.finite_p d_err
						then d_err
						else n.d_err) in
				   let c = (make_child s g nd h d h_err d_err
					      init h_err d_err f) in
				     if  f < !best_f then
				       (best_child := c;
					best_f := f)
				     else if f = !best_f then
				       (if d < !best_child.d then
					  (best_child := c;
					   best_f := f));
				     c)
			 (expand n.data n.g))
       in
	 if not ((List.length children) = 0)
	 then
	   (calc_hd_data n !best_child children;
	    List.iter update children);
	 children)



let find_incumbent_dups info key equals hash root goal_p bound expand =
  let openlist = (Geq.create_with est_f_then_d_then_g d_then_f_then_g
		    (make_close_enough bound) set_fhpos get_fhpos root)
  and closed = (Htable.create hash equals 250)
  and i = info in

  let insert n state =
    n.geqe <- Geq.insert openlist n;
    Htable.replace closed state n.geqe in

  let add_node n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else
      (let state = key n in
	 try
	   let entry = Htable.find closed state in
	     Limit.incr_dups i;
	     let prev = Geq.data entry in
	       if not (better_p prev n)
	       then (if prev.fh_pos <> Dpq.no_position
		     then (Geq.remove openlist (prev.geqe);
			   insert n state)
		     else insert n state)
	 with Not_found ->
	   insert n state) in

  let do_expand n =
    set_fhpos n Dpq.no_position;
    if not (Limit.promising_p i n)
    then (Limit.incr_prune i;
	  [])
    else
      (let children = expand n in
	 Limit.incr_exp i;
	 children) in

  let rec do_loop () =
    if ((Geq.count openlist) > 0) && not (Limit.halt_p i)
    then
      (let n = Geq.remove_best openlist in
	 if goal_p n
	 then Limit.new_incumbent i (Limit.Incumbent (0.,n))
	 else (let children = do_expand n in
		 List.iter add_node children;
		 Limit.curr_q i (Geq.count openlist);
		 do_loop())) in

    root.geqe <- (Geq.insert openlist root);
    Htable.add closed (key root) root.geqe;
    do_loop();
    openlist, closed


let cleanup_dups bound geq closed key goal_p expand info =
  let inc = (match info.Limit.incumbent with
	       | Limit.Nothing -> failwith "Cleanup without inc?"
	       | Limit.Incumbent (_, inc) -> inc) in
  let fq = Dpq.create f_order set_qpos 100 inc in

  let insert n state =
    n.geqe <- Geq.insert geq n;
    Htable.replace closed state n.geqe in

  let add_node n =
    Limit.incr_gen info;
    if not (Limit.promising_p info n)
    then Limit.incr_prune info
    else
      (let state = key n in
	 try
	   let entry = Htable.find closed state in
	     Limit.incr_dups info;
	     let prev = Geq.data entry in
	       if not (better_p prev n)
	       then (if prev.q_pos <> Dpq.no_position
		     then (Dpq.swap fq prev.q_pos n;
			   Geq.remove geq prev.geqe;
			   insert n state)
		     else (insert n state;
			   Dpq.insert fq n))
	 with Not_found ->
	   (insert n state;
	    Dpq.insert fq n)) in

  let rec cleanup_search () =
    if not (Limit.halt_p info) && not (Dpq.empty_p fq)
    then
      (let n = Dpq.extract_first fq in
	 if goal_p n
	 then Limit.new_incumbent info (Limit.Incumbent (bound, n))
	 else (if n.f *. bound < inc.f
	       then (let children = expand n in
		       List.iter add_node children;
		       cleanup_search ())
	       else (Limit.new_incumbent info
		       (Limit.Incumbent (bound, inc))))) in
    Geq.iter (fun node ->
		if node.f < inc.f
		then Dpq.insert fq node) geq;
    cleanup_search ()


let cleanup_2_dups  bound geq closed key goal_p expand info =
  let inc = (match info.Limit.incumbent with
	       | Limit.Nothing -> failwith "Cleanup with no incumbent?"
	       | Limit.Incumbent (_, inc) -> inc) in
  let fq = Dpq.create f_order set_qpos 100 inc in

  let insert n state =
    Dpq.insert fq n;
    let ge = Geq.insert geq n in
      set_geqe n ge;
      Htable.replace closed state ge in

  let get_node () =
    let best_f = Dpq.peek_first fq
    and best_fh = Geq.peek_doset geq
    and best_d = Geq.peek_best geq in
    let lb = best_f.f *. bound in
      if best_d.est_f < lb
      then best_d
      else (if best_fh.est_f < lb
	    then best_fh
	    else best_f) in

  let add_node n =
    Limit.incr_gen info;
    if not (Limit.promising_p info n)
    then Limit.incr_prune info
    else
      (let state = key n in
	 try
	   let entry = Htable.find closed state in
	     Limit.incr_dups info;
	     let prev = Geq.data entry in
	       if not (better_p prev n)
	       then (if (get_qpos prev) <> Dpq.no_position
		     then (Dpq.remove fq prev.q_pos;
			   Geq.remove geq prev.geqe;
			   insert n state)
		     else (insert n state))
	 with Not_found ->
	   insert n state) in


  let rec cleanup_search () =
    if not (Limit.halt_p info) && not (Dpq.empty_p fq)
    then
      (let n = get_node () in
	 Geq.remove geq n.geqe;
	 Dpq.remove fq n.q_pos;
	   if not (Limit.promising_p info n)
	   then (Limit.incr_prune info;
		 cleanup_search ())
	   else if goal_p n
	   then (Limit.new_incumbent info (Limit.Incumbent (bound, n)))
	   else (if (Math.fmin n.f (Dpq.peek_first fq).f) *. bound < inc.f
		 then (let children = expand n in
			 Limit.incr_exp info;
			 List.iter add_node children;
			 cleanup_search ())
		 else (Limit.new_incumbent info
			 (Limit.Incumbent (bound, inc))))) in
    Geq.iter (fun node -> Dpq.insert fq node) geq;
    cleanup_search ()



let dups bound sface calc_hd_data fd_calc =
  let key n = sface.Search_interface.key n.data
  and hash = sface.Search_interface.hash
  and equals = sface.Search_interface.equals
  and goal_p n = sface.Search_interface.goal_p n.data
  and expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.hd calc_hd_data fd_calc)
  and root = (make_initial sface.Search_interface.initial
		sface.Search_interface.hd)
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on better_p
		(Limit.make_default_logger (fun n -> n.f)
		   (wrap sface.Search_interface.get_sol_length))) in
  let geq, closed = (find_incumbent_dups info key equals hash root goal_p
		       bound expand) in
    if not (Limit.halt_p info)
    then cleanup_dups bound geq closed key goal_p expand info;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


let dups2 bound sface calc_hd_data fd_calc =
  let key n = sface.Search_interface.key n.data
  and hash = sface.Search_interface.hash
  and equals = sface.Search_interface.equals
  and goal_p n = sface.Search_interface.goal_p n.data
  and expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.hd calc_hd_data fd_calc)
  and root = (make_initial sface.Search_interface.initial
		sface.Search_interface.hd)
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on better_p
		(Limit.make_default_logger (fun n -> n.f)
		   (wrap sface.Search_interface.get_sol_length))) in
  let geq, closed = (find_incumbent_dups info key equals hash root goal_p
		       bound expand) in
    if not (Limit.halt_p info)
    then cleanup_2_dups bound geq closed key goal_p expand info;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


let dups2_pm bound sface calc_hd_data fd_calc =
  let key n = sface.Search_interface.key n.data
  and hash = sface.Search_interface.hash
  and equals = sface.Search_interface.equals
  and goal_p n = sface.Search_interface.goal_p n.data
  and expand = (make_expand_pathmax sface.Search_interface.domain_expand
		  sface.Search_interface.hd calc_hd_data fd_calc)
  and root = (make_initial sface.Search_interface.initial
		sface.Search_interface.hd)
  and info = (Limit.make Limit.Nothing sface.Search_interface.halt_on better_p
		(Limit.make_default_logger (fun n -> n.f)
		   (wrap sface.Search_interface.get_sol_length))) in
  let geq, closed = (find_incumbent_dups info key equals hash root goal_p
		       bound expand) in
    if not (Limit.halt_p info)
    then cleanup_2_dups bound geq closed key goal_p expand info;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


(* EOF *)

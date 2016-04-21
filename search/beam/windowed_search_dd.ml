(** A duplicate delaying version of windowed search. It differs in basically
    one line, causing nodes which are re-encountered on a current itteration
    to be suspended intsead of re-expanded. *)

let swap_in_best hashtable key better considered c_window =
  try
    let (prev,p_window) = Htable.find hashtable (key considered) in
      if better considered prev
      then Htable.replace hashtable (key considered) (considered,c_window)
  with Not_found -> Htable.add hashtable (key considered)
    (considered,c_window)


let default_in_window node_value current_window window_size =
  node_value <= current_window -. window_size


let do_windowed_iteration ?(win_fn = default_in_window)
    ?(get_next_window = Math.fmax) closed_list
    suspend_list open_list open_ht better_p is_goal expand key get_window
    get_ind get_cost i window_size =
  assert (window_size > 0.);
  let goaled = ref false
  and level = ref (get_window (Dpq.peek_first open_list)) in
  let consider_child n =
    let in_open = Htable.mem open_ht (key n)
    and in_closed = Htable.mem closed_list (key n)
    and in_delay = Htable.mem suspend_list (key n) in
      Limit.incr_gen i;
      if not (in_open || in_closed || in_delay)
      then (Htable.replace open_ht (key n) (n,window_size);
	    Dpq.insert open_list n;)
	(* node is in the open list and is
	   better than the one that's currently there.*)
      else (if (in_open &&
		  (better_p n (fst(Htable.find open_ht (key n)))))
	    then
	      (let old_node, old_ind = Htable.find open_ht (key n) in
		 Htable.replace open_ht (key n) (n,window_size);
		 Dpq.swap open_list (get_ind old_node) n;)
	    else (if (in_delay &&
			(better_p n (fst(Htable.find suspend_list (key n)))))
		  then
		    ((*Htable.replace suspend_list (key n) (n,window_size);*)
		      Htable.replace open_ht (key n) (n,window_size);
		      Dpq.insert open_list n)
		  else (if in_closed
			then
			  (let prev,pind = Htable.find closed_list (key n)
			   in
			     if (better_p n prev) && (pind <> window_size)
			     then (Limit.incr_dups i;
				   Htable.replace open_ht (key n) (n,window_size);
				   Dpq.insert open_list n)
			     else (if (better_p n prev)
				   then (Htable.replace suspend_list (key n)
					   (n,window_size))))))) in

    while (not (Dpq.empty_p open_list)) && (not (Limit.halt_p i))
      && (not !goaled)
    do
      (let next_node = Dpq.extract_first open_list in
       let next_window = get_window next_node in
	 (*Verb.pe Verb.debug "Level: %f\tNext_Window: %f\n" !level next_window;*)
	 ignore (Htable.remove open_ht (key next_node));
	 swap_in_best closed_list key better_p next_node window_size;
	 if (Limit.promising_p i next_node)
	 then
	   (if win_fn next_window !level window_size
	    then (Htable.remove closed_list (key next_node);
		  swap_in_best suspend_list key better_p next_node window_size)
	    else
	      (level := get_next_window next_window !level;
	       if(is_goal next_node)
	       then ((match i.Limit.incumbent with
			| Limit.Nothing -> goaled := true
			| Limit.Incumbent (q,inc) ->
			    goaled := better_p next_node inc);
		     Limit.new_incumbent i
		       (Limit.Incumbent ((get_cost next_node), next_node)))
	       else (List.iter consider_child (expand next_node);
		     Limit.incr_exp i))))
    done

(* EOF *)

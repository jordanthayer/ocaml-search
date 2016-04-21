(** Windowed Search, adapted from Chris' implementation of the anytime
    variant *)

(* will need to change the idea that windows are only integers.
   will need to change the idea that they are always growing
   maybe tomorrow - Jordan *)

let swap_in_best hashtable key better considered =
  try
    let prev = Htable.find hashtable (key considered) in
      if better considered prev
      then Htable.replace hashtable (key considered) considered
  with Not_found -> Htable.replace hashtable (key considered) considered


let default_in_window node_value current_window window_size =
  (*Math.within node_value current_window window_size*)
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
      then (Htable.replace open_ht (key n) n; (*this node is really new *)
	    Dpq.insert open_list n;)
	(* node is in the open list and is
	   better than the one that's currently there.*)
      else (if (in_open &&
		  (better_p n (Htable.find open_ht (key n))))
	    then
	      (let old_node = Htable.find open_ht (key n) in
		 Htable.replace open_ht (key n) n;
		 Dpq.swap open_list (get_ind old_node) n;)
	    else (if (in_delay &&
			(better_p n (Htable.find suspend_list (key n))))
		  then (Htable.replace suspend_list (key n) n;)
		  else (if(in_closed &&
			     (better_p n
				(Htable.find closed_list (key n))))
			then (Limit.incr_dups i;
			      Htable.replace open_ht (key n) n;
			      Dpq.insert open_list n;)))) in

    while (not (Dpq.empty_p open_list)) && (not (Limit.halt_p i))
      && (not !goaled)
    do
      (let next_node = Dpq.extract_first open_list in
       let next_window = get_window next_node in
	 (*Verb.pe Verb.debug "Level: %f\tNext_Window: %f\n" !level next_window;*)
	 ignore (Htable.remove open_ht (key next_node));
	 swap_in_best closed_list key better_p next_node;
	 if (Limit.promising_p i next_node)
	 then
	   (if win_fn next_window !level window_size
	    then (Htable.remove closed_list (key next_node);
		  swap_in_best suspend_list key better_p next_node;)
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

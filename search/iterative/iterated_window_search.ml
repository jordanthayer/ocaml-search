(** Converts windowed searches into anytime searches by iteratively
    increasing the size of the window that the algorithms are being run
    with. *)

let alt_col_name = "approximate_quality"
(* the f-value of any node on open may be inflated due to inadmissible
   pruning techniques.  *)

let output_col_hdr () =
  Datafile.write_alt_colnames stdout alt_col_name ["iteration";
						   "quality";]

let print i qual =
  Datafile.write_alt_row_prefix stdout alt_col_name;
  Verb.pr Verb.always "%f\t%f\n" i qual


let do_search
    ?(queue_record = Recorders.none)
    ?(win_fn = Windowed_search.default_in_window)
    ?(get_next_window = Math.fmax) ?(step = 1.)
    ?(init_window = 1.)
    i initial is_goal expand ordered_p node_eval_fun
    hash_compare hash key set_ind get_ind get_cost get_window =
  output_col_hdr ();
  let window_size = ref init_window in
  let closed_list = Htable.create hash hash_compare 100 in
  let suspend_list = Htable.create hash hash_compare 100 in
  let open_ht = Htable.create hash hash_compare 100 in
  let open_list = Dpq.create ordered_p set_ind
    100 initial
  and max_min_f = ref initial in
  let do_iteration = (Windowed_search.do_windowed_iteration ~win_fn:win_fn
			~get_next_window:get_next_window
			closed_list suspend_list open_list open_ht
			node_eval_fun is_goal expand key get_window get_ind
			get_cost i) in
    Dpq.insert open_list initial;
    Htable.add open_ht (key initial) initial;
    Verb.pe Verb.debug "Starting First Iteration\n";
    do_iteration !window_size;
    Verb.pe Verb.debug "First iteration done!\n";
    Verb.pe Verb.debug "%b\t%b\n" (not(Limit.halt_p i)) ((Htable.length suspend_list) != 0);
    while ((not(Limit.halt_p i)) && ((Htable.length suspend_list) != 0))
    do
      (match i.Limit.incumbent with
	 | Limit.Nothing ->
	     Verb.pe Verb.debug "%f\tNo Solution\n" !window_size;
	 | Limit.Incumbent (c,n) ->
	     Verb.pe Verb.debug "%f\t%f\n" !window_size c;);
      window_size := !window_size +. step;
      (* adds open list to the closed list *)
      Htable.iter (fun k n ->
		     try
		       let prev = Htable.find closed_list k in
			 if node_eval_fun n prev
			 then Htable.replace closed_list k n
		     with Not_found -> Htable.replace closed_list k n) open_ht;
      (* clears the open list *)
      Htable.clear open_ht;
      Dpq.clear open_list;
      (* put the suspended list into the open list *)
      Htable.iter (fun _ n -> Htable.replace open_ht (key n) n;
		     Dpq.insert open_list n;) suspend_list;
      let best = Dpq.peek_first open_list in
	max_min_f := (if (get_cost best) > (get_cost !max_min_f)
		      then best else !max_min_f);
	(match i.Limit.incumbent with
	   | Limit.Nothing ->
	      print (!window_size -. init_window) infinity;
	  | Limit.Incumbent (c,n) ->
	      print (!window_size -. init_window)
		((get_cost n) /. (get_cost !max_min_f)));
	Htable.clear suspend_list;
	do_iteration !window_size
    done


let do_search_dd ?(queue_record = Recorders.none)
    ?(win_fn = Windowed_search.default_in_window)
    ?(get_next_window = Math.fmax) ?(step = 1.)
    ?(init_window = 1.)
    i initial is_goal expand ordered_p node_eval_fun
    hash_compare hash key set_ind get_ind get_cost get_window =
  output_col_hdr ();
  let window_size = ref init_window in
  let closed_list = Htable.create hash hash_compare 100 in
  let suspend_list = Htable.create hash hash_compare 100 in
  let open_ht = Htable.create hash hash_compare 100 in
  let open_list = Dpq.create ordered_p set_ind
    100 initial
  and max_min_f = ref initial in
  let do_iteration = (Windowed_search_dd.do_windowed_iteration ~win_fn:win_fn
			~get_next_window:get_next_window
			closed_list suspend_list open_list open_ht
			node_eval_fun is_goal expand key get_window get_ind
			get_cost i) in
    Dpq.insert open_list initial;
    Htable.add open_ht (key initial) (initial,init_window);
    Verb.pe Verb.debug "Starting First Iteration\n";
    do_iteration !window_size;
    Verb.pe Verb.debug "First iteration done!\n";
    Verb.pe Verb.debug "%b\t%b\n" (not(Limit.halt_p i)) ((Htable.length suspend_list) != 0);
    while ((not(Limit.halt_p i)) && ((Htable.length suspend_list) != 0))
    do
      (match i.Limit.incumbent with
	 | Limit.Nothing ->
	     Verb.pe Verb.debug "%f\tNo Solution\n" !window_size;
	 | Limit.Incumbent (c,n) ->
	     Verb.pe Verb.debug "%f\t%f\n" !window_size c;);
      window_size := !window_size +. step;
      (* adds open list to the closed list *)
      Htable.iter (fun k (n,ni) ->
		     try
		       let prev,pi = Htable.find closed_list k in
			 if node_eval_fun n prev
			 then Htable.replace closed_list k (n,!window_size)
		     with Not_found -> Htable.replace closed_list k
		       (n,!window_size)) open_ht;
      (* clears the open list *)
      Htable.clear open_ht;
      Dpq.clear open_list;
      (* put the suspended list into the open list *)
      Htable.iter (fun _ (n,ni) -> Htable.add open_ht (key n) (n,ni);
		     Dpq.insert open_list n;) suspend_list;
      let best = Dpq.peek_first open_list in
	max_min_f := (if (get_cost best) > (get_cost !max_min_f)
		      then best else !max_min_f);
	(match i.Limit.incumbent with
	   | Limit.Nothing ->
	      print (!window_size -. init_window) infinity;
	  | Limit.Incumbent (c,n) ->
	      print (!window_size -. init_window)
		((get_cost n) /. (get_cost !max_min_f)));
      Htable.clear suspend_list;
      do_iteration !window_size
    done

(* EOF *)


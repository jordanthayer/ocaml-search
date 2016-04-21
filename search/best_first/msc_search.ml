(** Multi-State commitment framework - By Kitamura, Yokoo, Miyaji, Tatsumi **)

let init_lists_no_dups sface size pred setpos =
  (* constructs the data structures for no dups *)
  let initial = sface.Search_interface.initial in
  let reserve = Dpq.create pred setpos 100 initial
  and commit = Mmh.create pred size initial in
    ignore(Mmh.insert commit initial);
    commit, reserve


let init_lists_dups ?(on_exp = false) sface size pred setpos =
  (* constructs the data structures in the duplicates situation *)
  let key = sface.Search_interface.key
  and initial = sface.Search_interface.initial in
  let reserve = Dpq.create pred setpos 100 initial
  and commit = Mmh.create pred size initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100 in
    ignore (Mmh.insert commit initial);
    if not on_exp then Htable.add nodes (key initial) initial;
    commit, reserve, nodes


let refill commit reserve =
  (* if the commit list is undersized, this will fill replenish it with
     nodes from the reserve list *)
    while ((commit.Mmh.heap_count < commit.Mmh.heap_size) &&
	     (Dpq.count reserve) > 0)
    do
      ignore (Mmh.insert commit (Dpq.extract_first reserve));
    done


let insert_no_dups commit reserve n =
  (* inserts node [n] into the appropriate list *)
  match Mmh.insert commit n with
      [] -> ()
    | n :: [] -> Dpq.insert reserve n
    | n :: tl -> failwith "MMH returned more than one item"


let insert_dups_at_expand commit reserve closed n =
  insert_no_dups commit reserve n


let insert_dups_at_insert getpos key better_p commit reserve closed n =
  try
    let prev = Htable.find closed (key n) in
      if not (better_p prev n)
      then (Htable.replace closed (key n) n;
	    let ppos = getpos prev in
	      if ppos <> Dpq.no_position
	      then (if (Dpq.get_at reserve ppos) != prev
		    then (assert ((Mmh.access_at commit ppos) == prev);
			  ignore (Mmh.replace_at commit n ppos))
		    else Dpq.swap reserve ppos n))
  with Not_found ->
    Htable.add closed (key n) n;
    insert_no_dups commit reserve n


let rec get_best_k mmh k =
  (** Returns the best [k] elements from the min-max heap
      Useful for implementing MSC-kwA* *)
  if (k == 0) || (Mmh.empty_p mmh)
  then []
  else (let n = Mmh.extract_first mmh in
	  n::(get_best_k mmh (k - 1)))



let search k size sface ordered_p better_p =
  (** Needs some commenting *)
  let commit,reserve = init_lists_no_dups sface size ordered_p (fun _ _ -> ())
  and i = sface.Search_interface.info in
    let rec expand_best () =
      if (not (Mmh.empty_p commit)) && (not (Limit.halt_p i))
      then
	(let next = get_best_k commit k  in
	   List.iter
	     (fun n ->
		if not (Limit.promising_p i n) then
		  (Limit.incr_prune i;
		   expand_best ())
		else if sface.Search_interface.goal_p n then
		  Limit.new_incumbent i (Limit.Incumbent (0.,n))
		else
		  (Limit.incr_exp i;
		   List.iter (fun n ->
				Limit.incr_gen i;
				if Limit.promising_p i n
				then insert_no_dups commit reserve n
				else Limit.incr_prune i)
		     (sface.Search_interface.node_expand n);
		   Limit.curr_q i ((Mmh.count commit) + (Dpq.count reserve))))
	     next;
	   refill commit reserve;
	   expand_best()) in
      expand_best ();
      Limit.results5 i


let search_dups_expand k size sface ordered_p better_p setpos getpos =
  (** Needs some commenting *)
  let commit,reserve,closed = init_lists_dups ~on_exp:true sface size
    ordered_p setpos
  and i = sface.Search_interface.info in
    let rec expand_best () =
      if (not (Mmh.empty_p commit)) && (not (Limit.halt_p i))
      then
	(let next = get_best_k commit k  in
	   List.iter
	     (fun n ->
		setpos n Dpq.no_position;
		if not (Limit.promising_p i n)
		then (Limit.incr_prune i;
		      expand_best ())
		else if sface.Search_interface.goal_p n then
		  Limit.new_incumbent i (Limit.Incumbent (0.,n))
		else if not (Htable.mem closed (sface.Search_interface.key n))
		then
		  (Htable.add closed (sface.Search_interface.key n) n;
		   Limit.incr_exp i;
		   List.iter (fun n ->
				Limit.incr_gen i;
				if Limit.promising_p i n
				then insert_dups_at_expand commit reserve closed n
				else Limit.incr_prune i)
		     (sface.Search_interface.node_expand n);
		   Limit.curr_q i ((Mmh.count commit) + (Dpq.count reserve))))
	     next;
	   refill commit reserve;
	   expand_best()) in
      expand_best ();
      Limit.results6 i


let search_dups_insert k size sface ordered_p better_p setpos getpos =
  (** Needs some commenting *)
  let commit,reserve,closed = init_lists_dups sface size
    ordered_p setpos
  and i = sface.Search_interface.info in
  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n)
    then Limit.incr_prune i
    else (insert_dups_at_insert getpos sface.Search_interface.key better_p
	    commit reserve closed n)
  in
  let rec consider_node nlist =
    match nlist with
      | [] -> true
      | hd::tl -> (setpos hd Dpq.no_position;
		   if not (Limit.promising_p i hd)
		   then (Limit.incr_prune i;
			 consider_node tl)
		   else if sface.Search_interface.goal_p hd
		   then (Limit.new_incumbent i (Limit.Incumbent (0.,hd));
			 false)
		   else
		     (Limit.incr_exp i;
		      List.iter consider_child (sface.Search_interface.node_expand hd);
		      Limit.curr_q i ((Mmh.count commit) + (Dpq.count reserve));
		      consider_node tl)) in
  let rec expand_best () =
    if (not (Mmh.empty_p commit)) && (not (Limit.halt_p i))
    then
      (let next = get_best_k commit k in
	 if consider_node next
	 then (refill commit reserve;
	       expand_best())) in
    expand_best ();
    Limit.results6 i

(* EOF *)

(** Optimistic Framework Code, brought into the search interface.
    Allows for reordering of the optimistic list
    Jordan - August 2009 *)

let delayed = -2
exception Done
exception NoIncumbent


let no_record = (fun _ _ _ -> ())

(******************** aggressive expansion orderings *************************)

let rec expand_prime ?(record = no_record) update_open pq i sface =
  (** Expands nodes from the optimistic queue until an incumbent solution
      is in hand.  Used for domains with few / no duplicates *)
  if (not (Dpq.empty_p pq)) && (not (Limit.halt_p i))
  then
    (record i pq pq;
     let n = Dpq.extract_first pq in
       if not (Limit.promising_p i n) then
	 (Limit.incr_prune i;
	  expand_prime update_open pq i sface)
       else if sface.Search_interface.goal_p n (* must be new best *)
       then
	 ((*Verb.pe Verb.toplvl "Found Goal\n%!";*)
	  Limit.new_incumbent i (Limit.Incumbent (0.,n)))
       else
	 (Limit.incr_exp i;
	  let reorder, children = sface.Search_interface.resort_expand n in
	    if reorder then update_open pq;
	    List.iter (fun c ->
			 Limit.incr_gen i;
			 if Limit.promising_p i c then
			   Dpq.insert pq c
			 else
			   Limit.incr_prune i)
	      children;
	    Limit.curr_q i (Dpq.count pq);
	    expand_prime update_open pq i sface))


let rec expand_prime_dups ?(record = no_record) update_open pq i sface closed
    better_p get_p_pos =
  (** Expands nodes from the optimistic queue until an incumbent solution
      is in hand.  Used for domains with many duplicates *)
  let consider_child n =
    let state = sface.Search_interface.key n in
      try
	let prev = Htable.find closed state in
	  Limit.incr_dups i;
	  if not (better_p prev n) then
	    (Htable.replace closed state n;
	     if (get_p_pos prev) = Dpq.no_position then
	       Dpq.insert pq n
	     else Dpq.swap pq (get_p_pos prev) n)
      with Not_found ->
	Dpq.insert pq n;
	Htable.add closed state n in

    if (not (Dpq.empty_p pq)) && (not (Limit.halt_p i))
    then
      (record i pq pq;
       let n = Dpq.extract_first pq in
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    expand_prime_dups update_open pq i sface closed better_p get_p_pos)
	 else if sface.Search_interface.goal_p n then
	   (* must be new best *)
	   Limit.new_incumbent i (Limit.Incumbent (0.,n))
	 else
	   (Limit.incr_exp i;
	    let reorder, children = sface.Search_interface.resort_expand n in
	      if reorder then update_open pq;
	      List.iter (fun n ->
			   Limit.incr_gen i;
			   if Limit.promising_p i n then
			     consider_child n
			   else
			     Limit.incr_prune i)
		children;
	      Limit.curr_q i (Dpq.count pq);
	      expand_prime_dups update_open pq i sface closed better_p
		get_p_pos))


let rec expand_prime_delay_dups ?(record = no_record) update_open pq i sface
    closed dpq better_p get_p_pos get_d_pos =
  (** Expands nodes from the optimistic queue until an incumbent solution
      is in hand.  Used for domains with many duplicates.  Duplicates are
      placed in an additional queue and not considered until cleanup *)
  let consider_child n =
    let state = sface.Search_interface.key n in
      try
	let prev = Htable.find closed state in
	  Limit.incr_dups i;
	  if not (better_p prev n) then
	    (Htable.replace closed state n;
	     let ppos = (get_p_pos prev) in
	       if ppos = Dpq.no_position
	       then (let dpos = (get_d_pos prev) in
		       if dpos = Dpq.no_position
		       then Dpq.insert dpq n
		       else try
			 Dpq.swap dpq dpos n
		       with Invalid_argument str -> failwith "delay q swap")
	       else Dpq.swap pq ppos n)
      with Not_found ->
	Dpq.insert pq n;
	Htable.add closed state n in
    if (not (Dpq.empty_p pq)) && (not (Limit.halt_p i)) then
      (record i pq pq;
       let n = Dpq.extract_first pq in
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    expand_prime_delay_dups update_open pq i sface closed dpq better_p
	      get_p_pos get_d_pos)
	 else if sface.Search_interface.goal_p n then
	   (* must be new best *)
	   Limit.new_incumbent i (Limit.Incumbent (0.,n))
	 else
	   (Limit.incr_exp i;
	    let reorder, children = sface.Search_interface.resort_expand n in
	      if reorder then update_open pq;
	      List.iter (fun n ->
			   Limit.incr_gen i;
			   if Limit.promising_p i n then
			     consider_child n
			   else
			     Limit.incr_prune i)
		children;
	      Limit.curr_q i (Dpq.count pq);
	      expand_prime_delay_dups update_open pq i sface closed dpq
		better_p get_p_pos get_d_pos))



(*********************** cleanup expansion orderings *************************)


let rec expand_f ?(record = no_record) update_open sface get_node fq pq i
    bound =
  (** expand_f manipulates both fq and pq **)
  if (not (Dpq.empty_p fq)) && (not (Limit.halt_p i))
  then
    (record i pq fq;
     let n = get_node fq pq i bound in
       if not (Limit.promising_p i n) then
	 (Limit.incr_prune i;
	  expand_f update_open sface get_node fq pq i bound)
       else if sface.Search_interface.goal_p n then
	 (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	  expand_f update_open sface get_node fq pq i bound)
       else
	 (Limit.incr_exp i;
	  let reorder, children = sface.Search_interface.resort_expand n in
	    if reorder then update_open pq;
	    List.iter (fun c ->
			 Limit.incr_gen i;
			 if Limit.promising_p i c then
			   (Dpq.insert pq c;
			    Dpq.insert fq c)
			 else
			   Limit.incr_prune i)
	      children;
	    Limit.curr_q i (Dpq.count fq);
	    expand_f update_open sface get_node fq pq i bound))


let rec expand_f_dups ?(record = no_record) update_open sface get_node fq pq
    closed i bound better_p get_p_pos get_f_pos =
  (** Cleanup phase of optimistic search for domains with duplicates. *)
  let sfq = Dpq.count fq
  and spq = Dpq.count pq in
    if sfq <> spq then failwith "mismatched queues";
    let consider_child n =
      let state = sface.Search_interface.key n in
	try
	  let prev = Htable.find closed state in
	    Limit.incr_dups i;
	    if not (better_p prev n) then
	      (Htable.replace closed state n;
	       if (get_p_pos prev) = Dpq.no_position then
		 (Dpq.insert pq n;
		  Dpq.insert fq n)
	       else
		 ((try
		     Dpq.swap pq (get_p_pos prev) n
		   with Invalid_argument str ->
		     failwith "pq swap");
		  (try
		     Dpq.swap fq (get_f_pos prev) n
		   with Invalid_argument str ->
		     failwith "fq swap")))
	with Not_found ->
	  (Dpq.insert pq n;
	   Dpq.insert fq n;
	   Htable.add closed state n) in
      (** expand_f manipulates both fq and pq **)
      if (not (Dpq.empty_p fq)) && (not (Limit.halt_p i)) then
	(record i pq fq;
	 let n = get_node fq pq i bound in
	   if not (Limit.promising_p i n) then
	     (Limit.incr_prune i;
	      expand_f_dups update_open sface get_node fq pq closed i bound
		better_p get_p_pos get_f_pos)
	   else if sface.Search_interface.goal_p n then
	     (Limit.new_incumbent i (Limit.Incumbent (0.,n));
	      expand_f_dups update_open sface get_node fq pq closed i bound
		better_p get_p_pos get_f_pos)
	   else
	     (Limit.incr_exp i;
	      let reorder, children = sface.Search_interface.resort_expand n in
		if reorder then update_open pq;
		List.iter (fun c ->
			     Limit.incr_gen i;
			     if Limit.promising_p i c
			     then consider_child c
			     else Limit.incr_prune i)
		  children;
		Limit.curr_q i (Dpq.count fq);
		expand_f_dups update_open sface get_node fq pq closed i bound
		  better_p get_p_pos get_f_pos))



(*************             queue builders                 *******************)


let make_fqueue ordered_f set_f_pos pq initial inc =
  (*make sure to only add nodes with f < incumebnt
    can cause queu inconsistencies later on.*)
  let fq = Dpq.create ordered_f set_f_pos (Dpq.count pq) initial in
    (match inc.Limit.incumbent with
	 Limit.Incumbent (q,i) ->
	   Dpq.iter
	     (fun n ->
		Dpq.insert fq n) pq
       | _ ->
	   Dpq.iter
	     (fun n -> Dpq.insert fq n) pq);
    fq


let make_delay_fqueue ordered_f set_f_pos dpq pq initial inc =
  (*make sure to only add nodes with f < incumebnt
    can cause queu inconsistencies later on.*)
  let fq = Dpq.create ordered_f set_f_pos (Dpq.count pq) initial in
  let fn = fun n -> Dpq.insert fq n in
    (match inc.Limit.incumbent with
	 Limit.Incumbent (q,i) ->
	   Dpq.iter fn pq;
	   Dpq.iter fn dpq;
	   Dpq.iter (fun n -> Dpq.insert pq n) dpq
       | _ ->
	   Dpq.iter fn pq;
	   Dpq.iter fn dpq;
	   Dpq.iter (fun n -> Dpq.insert pq n) dpq);
    fq

(****************************************************************************)

let no_dups sface update_open get_node ordered_p bound better_p ordered_f
    set_pq_pos set_f_pos =
  (** Performs optimistic searches for domains without duplicates *)
  let pq = Dpq.create ordered_p set_pq_pos 100 sface.Search_interface.initial;
  and i = sface.Search_interface.info in
    Dpq.insert pq sface.Search_interface.initial;
    (*Verb.pe Verb.toplvl "Beginning optimistic search\n%!";*)
    expand_prime update_open pq i sface;
    (match i.Limit.incumbent with
	 Limit.Incumbent(float,n) ->
	   (let fq = make_fqueue ordered_f set_f_pos pq
	      sface.Search_interface.initial i in
	      try
		(*Verb.pe Verb.toplvl "cleaningingup\n%!";*)
		expand_f update_open sface get_node fq pq i bound
	      with Done -> (match i.Limit.incumbent with
				Limit.Nothing -> failwith "Done but no incumbent?"
			      | Limit.Incumbent (q,n) ->
				  Limit.new_incumbent i (Limit.Incumbent (bound,n))))
       | _ -> ());
    Limit.results5 i


let dups sface update_open get_node ordered_p bound better_p ordered_f
    set_pq_pos set_f_pos get_p_pos get_f_pos =
  (** Performs optimistic searches for domains with duplicates *)
  let pq = Dpq.create ordered_p set_pq_pos 100 sface.Search_interface.initial;
  and i = sface.Search_interface.info
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100 in
    Dpq.insert pq sface.Search_interface.initial;
    (*Verb.pe Verb.toplvl "Beginning optimistic search\n%!";*)
    expand_prime_dups update_open pq i sface closed better_p get_p_pos;
    (match i.Limit.incumbent with
	 Limit.Incumbent(float,n) ->
	   (let fq = make_fqueue ordered_f set_f_pos pq
	      sface.Search_interface.initial i in
	      try
		(*Verb.pe Verb.toplvl "cleaningingup\n%!";*)
		expand_f_dups update_open sface get_node fq pq closed i bound
		  better_p get_p_pos get_f_pos
	      with Done -> (match i.Limit.incumbent with
			       Limit.Nothing -> failwith "Done but no incumbent?"
			     | Limit.Incumbent (q,n) ->
				 Limit.new_incumbent i (Limit.Incumbent (bound,n))))
       | _ -> ());
    Limit.results6 i


let delay sface update_open get_node ordered_p bound better_p ordered_f
    set_pq_pos set_d_pos set_f_pos get_p_pos get_d_pos get_f_pos =
  (** Performs optimistic searches for domains with duplicates which will be
      delayed until cleanup phase*)
  let pq = Dpq.create ordered_p set_pq_pos 100 sface.Search_interface.initial
  and dpq = Dpq.create ordered_p set_d_pos 100 sface.Search_interface.initial
  and i = sface.Search_interface.info
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100 in
    Dpq.insert pq sface.Search_interface.initial;
    Htable.add closed (sface.Search_interface.key
			 sface.Search_interface.initial)
      sface.Search_interface.initial;
    expand_prime_delay_dups update_open pq i sface closed dpq better_p
      get_p_pos get_d_pos;
    (match i.Limit.incumbent with
	 Limit.Incumbent(float,n) ->
	   (let fq = make_delay_fqueue ordered_f set_f_pos dpq pq
	      sface.Search_interface.initial i in
	    let inc = n in
	      try
		Limit.new_incumbent i (Limit.Incumbent (0.,inc));
		(*Verb.pe Verb.toplvl "Began cleanup: %d\n%!" i.Limit.expanded;*)
		expand_f_dups update_open sface get_node fq pq closed i bound
		  better_p get_p_pos get_f_pos
	      with Done -> (match i.Limit.incumbent with
				Limit.Nothing -> failwith "Done but no incumbent?"
			      | Limit.Incumbent (q,n) ->
				  Limit.new_incumbent i (Limit.Incumbent (bound,n))))
       | _ -> ());
    Limit.results6 i



(* EOF *)

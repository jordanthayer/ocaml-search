(** Anytime Focal searches based around anytime repairing a*
    Jordan, Feb 2010 *)

let no_record = (fun _ _ _ _ -> ())

let search ?(record = no_record) continue sface wtlist focal_order f_order
    make_good feasible better_p (set_fpi, get_fpi) (set_fi,get_fi) get_cost =
  (** Needs some serious commenting of the operands - on the dups and no
      dups version perhaps?
      Search itsealf should never be directly called.  Dups and No-dups
      should handle this.
      In general, this is a very standard path-repairing search as suggested
      by Likhachev et al *)

  let closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100
  and incos  = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100
  and pq = Geq.create_with f_order focal_order (make_good (List.hd wtlist))
    set_fpi get_fpi sface.Search_interface.initial
  and i = sface.Search_interface.info in

  let consider_child c =
    Limit.incr_gen i;
    if(match i.Limit.incumbent with
	   Limit.Nothing -> false
	 | Limit.Incumbent (q,n) -> not (feasible c n))
    then Limit.incr_prune i
    else
      (try let prev = Htable.find closed (sface.Search_interface.key c) in
       let p_data = Geq.data prev in
	 if not (better_p p_data c)
	   (* is prev on open? *)
	 then (if (get_fpi p_data) <> Dpq.no_position
	       then (Verb.pe Verb.never "Swapping into GEQ\n";
		     Htable.replace closed (sface.Search_interface.key c)
		       (Geq.swap pq prev c))
	       else (* prev not on open, was it expanded?*)
		 (try let p_incos = Htable.find incos
		    (sface.Search_interface.key c) in
		    Verb.pe Verb.never "Swapping in inconsistent\n";
		    if not (better_p p_incos c) then
		    Htable.replace incos (sface.Search_interface.key c) c
		  with Not_found ->
		    (Verb.pe Verb.never "Adding to inconsistent\n";
		     Htable.add incos (sface.Search_interface.key c) c)))
       with Not_found ->
	 (*Verb.pe Verb.never "Adding in fresh node\n";*)
	 Htable.add closed (sface.Search_interface.key c) (Geq.insert pq c)) in

  let rec improve_path wt =
    if ((not (Geq.empty_p pq)) && (not (Limit.halt_p i)))
    then
      (* remove s with smallest fp from open *)
      (let s = Geq.remove_best pq in
	 set_fpi s Dpq.no_position;
	 if not (Limit.promising_p i s)
	 then (Limit.incr_prune i;
	       improve_path wt)
	 else
	   (if sface.Search_interface.goal_p s
	    then (Verb.pe Verb.never "Goal!\n";
		  Limit.new_incumbent i (Limit.Incumbent (0.,s)))
	    else (Limit.incr_exp i;
		  List.iter consider_child
		    (sface.Search_interface.node_expand s);
		  improve_path wt)))

  and compute_eps wt =
    match i.Limit.incumbent with
	Limit.Nothing -> wt
      | Limit.Incumbent (q,n) ->
	  if Geq.empty_p pq
	  then wt
	  else min wt ((get_cost n) /. (get_cost  (Geq.peek_doset pq)))
  in

  let rec do_search wts eps_prime =
    if (not (Limit.halt_p i))
    then
      (match wts with
	   hd::tl ->
	     ((assert ((not (tl = [])) || hd = 1.));
	      if eps_prime > 1. ||
		(continue &&
		   not ((Geq.empty_p pq) && ((Htable.length incos) = 0)))
	      then
		(Verb.pe Verb.never "Iteration @ wt = %f\n" hd;
		 Geq.update_close_enough pq (make_good hd);
		 Htable.iter (fun key ele ->
				ignore (Geq.insert pq ele)) incos;
		 (* empty closed list *)
		 Htable.clear incos;
		 Htable.clear closed;
		 Geq.raw_iter (fun e -> Htable.add closed
				 (sface.Search_interface.key (Geq.data e)) e)
		   pq;
		 (* call improve path *)
		 improve_path hd;
		 if tl = [] then do_search [hd] (compute_eps hd)
		 else do_search tl (compute_eps hd))
	      else i (* last solution was optimal*))
	 | [] -> i)
    else i
  in
    Htable.add closed (sface.Search_interface.key
			 sface.Search_interface.initial)
      (Geq.insert pq sface.Search_interface.initial);
    Limit.incr_gen i;
    Verb.pe Verb.never "%i iterations\n" (List.length wtlist);
    improve_path (List.hd wtlist);
    do_search (List.tl wtlist) (compute_eps (List.hd wtlist))




let no_dups ?(continue = true) sface wtlist focal_order f_order make_close
    feasible better_p setget_fpi setget_fi get_cost =
  (** Performs the reparing search on domains with no or very few duplicate
      nodes.
      [sface] is the node search interface
      [wtlist] list of weights to be used during search in order of use
      [search_order] order in which nodes are expanded
      [f_order] g + h order of nodes
      [feasible] is this node able to produce something better than incumbent
      [better_p] determines if a node represents a better solution
      [update] updates the cost of a node based on the current weight
      [set_fpi] index setter
      [set_fi]  index setter
      [get_fpi] index getter
      [get_fi]  index getter
      [get_cost] gets cost (f cost) of a node *)
  Limit.results5
    (search continue sface wtlist focal_order f_order make_close
       feasible better_p setget_fpi setget_fi get_cost)


and dups ?(continue = false) sface wtlist focal_order f_order make_close
    feasible better_p setget_fpi setget_fi get_cost =
  (** Performs the reparing search on domains with no or very few duplicate
      nodes.
      [sface] is the node search interface
      [wtlist] list of weights to be used during search in order of use
      [search_order] order in which nodes are expanded
      [f_order] g + h order of nodes
      [feasible] is this node able to produce something better than incumbent
      [better_p] determines if a node represents a better solution
      [update] updates the cost of a node based on the current weight
      [set_fpi] index setter
      [set_fi]  index setter
      [get_fpi] index getter
      [get_fi]  index getter
      [get_cost] gets cost (f cost) of a node *)
  Limit.results6
    (search continue sface wtlist focal_order f_order make_close
       feasible better_p setget_fpi setget_fi get_cost)

(* EOF *)

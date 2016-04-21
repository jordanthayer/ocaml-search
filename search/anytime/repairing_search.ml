(** Repairing Search - An anytime approach where inconsistent nodes are
    repaired during the course of the search.

    Based on a retooling of the ARA* code.
    Ethan & Jordan June 9, '09 *)

let no_record = (fun _ _ _ _ -> ())


let search  ?(record = no_record) sface root wtlist search_order f_order
    feasible better_p update set_fpi set_fi get_fpi get_fi get_cost =
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
  and pq = Dpq.create search_order set_fpi 100 root
  and fq = Dpq.create f_order set_fi 100 root
  and i = sface.Search_interface.info in

  let consider_child c =
    Limit.incr_gen i;
    if not (Limit.promising_p i c)
    then Limit.incr_prune i
    else
      (try let prev = Htable.find closed (sface.Search_interface.key c) in
	 if better_p c prev
	   (* is prev on open? *)
	 then (Htable.replace closed (sface.Search_interface.key c) c;
	       if (get_fpi prev) <> Dpq.no_position
	       then (assert ((get_fi prev) <> Dpq.no_position);
		     Dpq.swap pq (get_fpi prev) c;
		     Dpq.swap fq (get_fi prev) c)
	       else (* prev not on open, was it expanded?*)
		 (if Htable.mem incos (sface.Search_interface.key c)
		  then (let previ = Htable.find incos
			  (sface.Search_interface.key c) in
			  assert (previ == prev);
			  Htable.replace incos
			    (sface.Search_interface.key c) c;
			  assert ((get_fi prev) <> Dpq.no_position);
			  Dpq.swap fq (get_fi prev) c)
		  else (Htable.add incos (sface.Search_interface.key c) c;
			Dpq.insert fq c)))
       with Not_found ->
	 Dpq.insert pq c;
	 Dpq.insert fq c;
	 Htable.add closed (sface.Search_interface.key c) c) in

  let improve_path wt =
    while ((not (Dpq.empty_p pq)) &&
	     (match i.Limit.incumbent with
		  Limit.Nothing -> true
		| Limit.Incumbent (q,n) ->
		    not (feasible n (Dpq.peek_first pq)))
	   && (not (Limit.halt_p i)))
    do
      (* remove s with smallest fp from open *)
      (record i pq fq incos;
       let s = Dpq.extract_first pq in
	 Dpq.remove fq (get_fi s);
	 if not (Limit.promising_p i s)
	 then Limit.incr_prune i
	 else
	   (if sface.Search_interface.goal_p s
	    then Limit.new_incumbent i (Limit.Incumbent (0.,s))
	    else (Limit.incr_exp i;
		  List.iter (fun c ->
			       update c wt; (* No access to wted expand *)
			       consider_child c)
		    (sface.Search_interface.node_expand s))))
    done

  and compute_eps wt =
    match i.Limit.incumbent with
	Limit.Nothing -> wt
      | Limit.Incumbent (q,n) ->
	  if Dpq.empty_p pq
	  then wt
	  else min wt ((get_cost n) /. (get_cost (Dpq.peek_first fq)))
  in

  let rec do_search wts eps_prime =
    if (not (Limit.halt_p i))
    then
      (match wts with
	   hd::tl ->
	     ((assert ((not (tl = [])) || hd = 1.));
	      if eps_prime > 1.
	      then
		((* update priorities for all open nodes *)
		  Dpq.iter (fun e -> update e hd;
			      Dpq.see_update pq (get_fpi e)) pq;
		  (* move states from incos into open *)
		  Htable.iter (fun key ele ->
				  assert ((get_fi ele) <> Dpq.no_position);
				  update ele hd;
				  Dpq.insert pq ele) incos;
		  Htable.clear incos;
		  (* empty closed list *)
		  Htable.clear closed;
		  Dpq.iter (fun e -> Htable.add closed
			      (sface.Search_interface.key e) e) pq;
		  (* call improve path *)
		  improve_path hd;
		  do_search tl (compute_eps hd))

	      else ((*Verb.pe Verb.toplvl "eps_prime < 1.\n%!";*) i))
	 | [] -> (*Verb.pe Verb.toplvl"Exhausted weights\n%!";*) i)
    else i
  in
    Dpq.insert pq root;
    Dpq.insert fq root;
    Htable.add closed (sface.Search_interface.key root) root;
    Limit.incr_gen i;
    improve_path (List.hd wtlist);
    do_search (List.tl wtlist) (compute_eps (List.hd wtlist))


let no_dups sface wtlist search_order f_order feasible better_p update
    set_fpi set_fi get_fpi get_fi get_cost =
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
    (search sface sface.Search_interface.initial wtlist search_order f_order
       feasible better_p update set_fpi set_fi get_fpi get_fi get_cost)


and dups sface wtlist search_order f_order feasible better_p update
    set_fpi set_fi get_fpi get_fi get_cost =
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
    (search sface sface.Search_interface.initial wtlist search_order f_order
       feasible better_p update set_fpi set_fi get_fpi get_fi get_cost)

(* EOF *)

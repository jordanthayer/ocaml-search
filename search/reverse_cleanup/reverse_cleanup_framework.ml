(** Generic framework for performing reverse cleanup search
    Jordan - August 2009 *)

let no_record = (fun _ _ _ -> ())


let rec exploratory_expansions record fq pq i sface update_pq continue
    get_ppos =
  (** Exploratory expanisons performs the first f-ordered expansions.
      [record] is for recording data structures (dpq's in this case)
      [fq] is the f-ordered queue
      [i] is the limit.t
      [sface] is the search interface
      [update_pq] - updates all nodes in the pq, causing a resort
      [continue] decides if we continue searching in f-order
      [get_ppos] - gets the position of nodes in the pqueue *)
  if (not (Dpq.empty_p fq)) && (not (Limit.halt_p i))
  then
    (record i fq fq;
     let next = Dpq.extract_first fq in
       Dpq.remove pq (get_ppos next);
       if not (Limit.promising_p i next)
       then (Limit.incr_prune i;
	     if continue i next fq pq
	     then exploratory_expansions record fq pq i sface update_pq
	       continue get_ppos
	     else false)
       else if sface.Search_interface.goal_p next
       then ((*Verb.pe Verb.toplvl "Found Goal\n";*)
	     Limit.new_incumbent i (Limit.Incumbent (0., next));
	     true)
       else
	 (Limit.incr_exp i;
	  let resort , children = sface.Search_interface.resort_expand next in
	    if resort then update_pq pq;
	    List.iter (fun child ->
			 Limit.incr_gen i;
			 if Limit.promising_p i child
			 then (Dpq.insert fq child;
			       Dpq.insert pq child)
			 else Limit.incr_prune i) children;
	    if continue i next fq pq
	    then exploratory_expansions record fq pq i sface update_pq continue
	      get_ppos
	    else false))
  else false



let rec solution_run record fq pq i sface update_pq get_fpos =
  (** Solution run expands nodes in the aggressive best first manner
      until a solution is encountered.
  [record] - records the various queues for use with the visualization tool
  [fq] - nodes sorted in f-order
  [pq] - nodes sorted in best first order
  [i] - Limit.t data structure
  [sface] - search interface
  [update_pq] - updates all nodes in the pq, causing a resort
  [get_fpos] - gets the position of nodes in the fqueue
  *)
  if (not (Dpq.empty_p fq)) && (not (Limit.halt_p i))
  then
    (record i fq pq;
     let next = Dpq.extract_first pq in
       Dpq.remove fq (get_fpos next);
       if not (Limit.promising_p i next)
       then (Limit.incr_prune i;
	     solution_run record fq pq i sface update_pq get_fpos)
       else if sface.Search_interface.goal_p next
       then ((*Verb.pe Verb.toplvl "Found Goal\n";*)
	     Limit.new_incumbent i (Limit.Incumbent (0., next)))
       else
	 (Limit.incr_exp i;
	  let resort,children = sface.Search_interface.resort_expand next in
	    if resort then update_pq pq;
	    List.iter (fun child ->
			 (Limit.incr_gen i;
			  if Limit.promising_p i child
			  then (Dpq.insert fq child;
				Dpq.insert pq child)
			  else Limit.incr_prune i)) children;
	    solution_run record fq pq i sface update_pq get_fpos))



let rec cleanup record fq pq i sface update_pq get_node =
  (** Cleanup is like the cleanup of optimistic search.  Additional nodes
      are expanded in order to prove the quality bounds of the incumbent
      solution.
      [record] - records the various queues for use with the visualization tool
      [fq] - nodes sorted in f-order
      [pq] - nodes sorted in best first order
      [i] - Limit.t data structure
      [sface] - search interface
      [update_pq] - updates all nodes in the pq, causing a resort
      [get_node] - returns the next node to expand and tells the search if it
                   needs to keep going *)
  if (not (Dpq.empty_p fq)) && (not (Limit.halt_p i))
  then
    (record i pq fq;
     let next, continue = get_node fq pq i in
       if continue (* Continue is false if solution is within bound *)
       then
	 (if not (Limit.promising_p i next)
	  then (Limit.incr_prune i;
		cleanup record fq pq i sface update_pq get_node)
	  else if sface.Search_interface.goal_p next
	  then (Limit.new_incumbent i (Limit.Incumbent (0., next));
		cleanup record fq pq i sface update_pq get_node)
	  else
	    (Limit.incr_exp i;
	     let resort, children = sface.Search_interface.resort_expand next
	     in (if resort then update_pq pq;
		 List.iter (fun child ->
			      (Limit.incr_gen i;
			       if Limit.promising_p i child
			       then (Dpq.insert fq child;
				     Dpq.insert pq child)
			       else Limit.incr_prune i)) children;
		 cleanup record fq pq i sface update_pq get_node))))


let search ?(record = no_record) sface get_node f_order best_order update_pq
    get_fpos get_ppos continue set_fpos set_ppos =
  (** Performs a reverse cleanup search for domains with few / no duplicates
      [record] - records the various queues for use with the visualization tool
      [sface] - search interface
      [get_node] returns the next node to expand and tells the search if it
                   needs to keep going
      [f_order] - ordering predicate for admissible cost function
      [best_order] - ordering predicate for best first order
      [update_pq] updates all nodes in the pq, causing a resort
      [get_fpos] gets the position of nodes in the fqueue
      [get_ppos] gets the position of nodes in the pqueue
      [continue] decides if we continue searching in f-order
      [set_fpos] sets the fposition of nodes
      [set_ppos] sets the pposition of nodes *)
  let fq = Dpq.create f_order set_fpos 100 sface.Search_interface.initial
  and pq = Dpq.create best_order set_ppos 100 sface.Search_interface.initial
  and i = sface.Search_interface.info in
    Dpq.insert fq sface.Search_interface.initial;
    Dpq.insert pq sface.Search_interface.initial;
    if not (exploratory_expansions record fq pq i sface update_pq
	      continue get_ppos )
    then (solution_run record fq pq i sface update_pq get_fpos;
	  cleanup record fq pq i sface update_pq get_node);
    Limit.results5 i

(* EOF *)

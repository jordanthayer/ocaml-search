(** Generic framework for performing reverse cleanup search for domains with
    many duplicate states. Jordan - August 2009 *)

let no_record = (fun _ _ _ -> ())


let rec exploratory_expansions record fq pq closed i sface update_pq continue
    better_p get_fpos get_ppos =
  (** Exploratory expanisons performs the first f-ordered expansions.
      [record] is for recording data structures (dpq's in this case)
      [fq] is the f-ordered queue
      [pq] is the estimated f-ordered queue
      [i] is the limit.t
      [sface] is the search interface
      [update_pq] - updates all nodes in the pq, causing a resort
      [continue] decides if we continue searching in f-order
      [better_p] is the current node better than the last node?
      [get_fpos] - gets the position of nodes in the fqueue
      [get_ppos] - gets the position of nodes in the pqueue *)
  let consider_child c =
    let state = sface.Search_interface.key c in
      try let prev = Htable.find closed state in
	Limit.incr_dups i;
	if not (better_p prev c)
	then (Htable.replace closed state c;
	      if (get_fpos prev) = Dpq.no_position
	      then ((*Verb.pe Verb.debug "Inserting duplicate child.\n";*)
		    Dpq.insert fq c;
		    Dpq.insert pq c;)
	      else ((*Verb.pe Verb.debug "Swapping better child in.\n";*)
		    Dpq.swap fq (get_fpos prev) c;
		    Dpq.swap pq (get_ppos prev) c))
      with Not_found ->
	Dpq.insert fq c;
	Dpq.insert pq c;
	Htable.add closed state c in

    if (not (Dpq.empty_p fq)) && (not (Limit.halt_p i))
    then
      (record i fq fq;
       let next = Dpq.extract_first fq in
	 (*Verb.pe Verb.debug "Removing node from pq to sync removal from fq\n";*)
	 Dpq.remove pq (get_ppos next);
	 if not (Limit.promising_p i next)
	 then (Limit.incr_prune i;
	       if continue i next fq pq
	       then exploratory_expansions record fq pq closed i sface
		 update_pq continue better_p get_fpos get_ppos
	       else false)
	 else if sface.Search_interface.goal_p next
	 then ((*Verb.pe Verb.toplvl "Found Goal\n";*)
	       Limit.new_incumbent i (Limit.Incumbent (0., next));
	       true)
	 else
	   (Limit.incr_exp i;
	    let _, children = sface.Search_interface.resort_expand next in
	      List.iter (fun child ->
			   Limit.incr_gen i;
			   if Limit.promising_p i child
			   then consider_child child
			   else Limit.incr_prune i) children;
	      if continue i next fq pq
	      then exploratory_expansions record fq pq closed i sface
		update_pq continue better_p get_fpos get_ppos
	      else false))
    else false



let rec solution_run record fq pq closed i sface update_pq get_fpos get_ppos
    better_p =
  (** Solution run expands nodes in the aggressive best first manner
      until a solution is encountered.
      [record] - records the various queues for use with visualization
      [fq] - nodes sorted in f-order
      [pq] - nodes sorted in best first order
      [i] - Limit.t data structure
      [sface] - search interface
      [update_pq] - updates all nodes in the pq, causing a resort
      [get_fpos] - gets the position of nodes in the fqueue
      [get_ppos] - gets the position of nodes in the pqueue
      [better_p] is the current node better than the last node?  *)
  let consider_child c =
    let state = sface.Search_interface.key c in
      try let prev = Htable.find closed state in
	Limit.incr_dups i;
	if not (better_p prev c)
	then (Htable.replace closed state c;
	      if (get_ppos prev) = Dpq.no_position
	      then ((*Verb.pe Verb.debug "Inserting Duplicate Child.\n";*)
		    Dpq.insert pq c;
		    Dpq.insert fq c;)
	      else ((*Verb.pe Verb.debug "Swapping in duplicate child\n";*)
		    Dpq.swap pq (get_ppos prev) c;
		    Dpq.swap fq (get_fpos prev) c))
      with Not_found ->
	Dpq.insert fq c;
	Dpq.insert pq c;
	Htable.add closed state c in

    if (not (Dpq.empty_p fq)) && (not (Limit.halt_p i))
    then
      (record i fq pq;
       let next = Dpq.extract_first pq in
	 Dpq.remove fq (get_fpos next);
	 if not (Limit.promising_p i next)
	 then (Limit.incr_prune i;
	       solution_run record fq pq closed i sface update_pq get_fpos
		 get_ppos better_p)
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
			    then consider_child child
			    else Limit.incr_prune i)) children;
	      solution_run record fq pq closed i sface update_pq get_fpos
		get_ppos better_p))



let rec cleanup record fq pq closed i sface update_pq get_node
    better_p get_fpos get_ppos =
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
      needs to keep going
      [get_fpos] - gets the position of nodes in the fqueue
      [get_ppos] - gets the position of nodes in the pqueue
      [better_p] is the current node better than the last node?  *)
  let consider_child c =
    let state = sface.Search_interface.key c in
      try let prev = Htable.find closed state in
	Limit.incr_dups i;
	if not (better_p prev c)
	then (Htable.replace closed state c;
	      if (get_ppos prev) = Dpq.no_position
	      then (Dpq.insert pq c;
		    Dpq.insert fq c;)
	      else (Dpq.swap pq (get_ppos prev) c;
		    Dpq.swap fq (get_fpos prev) c))
      with Not_found ->
	Dpq.insert fq c;
	Dpq.insert pq c;
	Htable.add closed state c in

    if (not (Dpq.empty_p fq)) && (not (Limit.halt_p i))
    then
      (record i pq fq;
       let next, continue = get_node fq pq i in
	 if continue (* Continue is false if solution is within bound *)
	 then
	   (if not (Limit.promising_p i next)
	    then (Limit.incr_prune i;
		  cleanup record fq pq closed i sface update_pq get_node
		    better_p get_fpos get_ppos)
	    else if sface.Search_interface.goal_p next
	    then (Limit.new_incumbent i (Limit.Incumbent (0., next));
		  cleanup record fq pq closed i sface update_pq get_node
		 better_p get_fpos get_ppos)
	    else
	      (Limit.incr_exp i;
	       let resort, children = sface.Search_interface.resort_expand next
	       in (if resort then update_pq pq;
		   List.iter (fun child ->
				(Limit.incr_gen i;
				 if Limit.promising_p i child
				 then consider_child child
				 else Limit.incr_prune i)) children;
		   cleanup record fq pq closed i sface update_pq get_node
		     better_p get_fpos get_ppos))))


let search ?(record = no_record) sface get_node f_order best_order update_pq
    get_fpos get_ppos continue set_fpos set_ppos better_p =
  (** Performs a reverse cleanup search
      [record] - records the various queues for use with the visualization tool
      [sface] - search interface
      [get_node] returns the next node to expand and tells the search if it
                   needs to keep going
      [f_order] - ordering predicate for admissible cost function
      [best_order] - ordering predicate for best first order
      [update_pq] updates all nodes in the pq, causing a resort
      [get_fpos] gets the position of nodes in the fqueue
      [get_ppos] gets the position of nodes in the fqueue
      [continue] decides if we continue searching in f-order
      [set_fpos] sets the fposition of nodes
      [set_ppos] sets the pposition of nodes
      [better_p] is the current node better than the last node? *)
  let fq = Dpq.create f_order set_fpos 100 sface.Search_interface.initial
  and pq = Dpq.create best_order set_ppos 100 sface.Search_interface.initial
  and closed = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100
  and i = sface.Search_interface.info in
    Dpq.insert fq sface.Search_interface.initial;
    Dpq.insert pq sface.Search_interface.initial;
    if not (exploratory_expansions record fq pq closed i sface
	      update_pq continue better_p get_fpos get_ppos)
    then (solution_run record fq pq closed i sface update_pq get_fpos
	  get_ppos better_p;
	  cleanup record fq pq closed i sface update_pq get_node
	 better_p get_fpos get_ppos);
    Limit.results6 i

(* EOF *)

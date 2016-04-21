(**

   Search algorihtm that does weighted A* until memory is exhausted,
   then clears the open list and the closed list and restarts from the
   current head of the open list.

*)
type 'a node = {
  data : 'a;          (* Data Payload *)
  wf: float;          (* Search order w/ a weighted heuristic value *)
  f : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  mutable pos : int;  (* Position info for dpq *)
}


let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let ordered_p a b =
  (** Ordered predicate used for search.  Compares f', then f, then g.
      true if a is better than b.
  *)
  (a.wf < b.wf) ||
  ((a.wf = b.wf) &&
   ((a.f < b.f) ||
    ((a.f = b.f) &&
     (a.g >= b.g))))


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  a.f <= b.f


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand expand h wt =
  (** Takes the domain [expand] function and a [h]euristic calculator.
      Needs the [wt] which will be applied to the heuristic.
      Creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) ->
		 let hv = h d in
		   { data = d;
		     wf = g +. wt *.hv;
		     f = g +. hv;
		     g = g;
		     depth = n.depth + 1;
		     pos = Dpq.no_position; }) (expand n.data n.g))


let closed_pos = -2


let init_lists pred sface setpos =
  let key = sface.Search_interface.key
  and initial = sface.Search_interface.initial in
  let openlist = Dpq.create pred setpos 100 initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100 in
    Dpq.insert openlist initial;
    Htable.add nodes (key initial) initial;
    openlist, nodes


let search_dups ?(record = Fn.no_op2) sface ordered_p better_p setpos getpos =
  (** [ordered_p] is true if in order or equal, [better_p] is true if
      superior or equal (eg, for comparing duplicates), [key] gives
      state data for detecting duplicates, [setpos] and [getpos] are
      for efficient swapping into the openlist.  This function is
      intended to be a building block for others - it expects an
      [expand] that takes the openlist as the first argument (in case
      [expand] wants to mess with it). *)
  (* openlist reflects ordering.  nodes has all nodes.  pos is [closedpos]
     for nodes on the closed list. *)
  let out_of_memory = Limit.make_halt_p [Limit.MachineMemory] in
  let openlist, nodes = init_lists ordered_p sface setpos
  and i = sface.Search_interface.info in
  let consider_child n =
    Limit.incr_gen i;
    if not (Limit.promising_p i n) then
      Limit.incr_prune i
    else
      let state = sface.Search_interface.key n in
	(* if heuristic is consistent (monotone) then
	   first instance of node is always best path. *)
	try
	  let prev = Htable.find nodes state in
	    Limit.incr_dups i;
	    if not (better_p prev n) then
	      (* better path to previous state *)
	      (Htable.replace nodes state n;
	       let pos = getpos prev in
		 if pos == closed_pos
		 then Dpq.insert openlist n
		 else Dpq.swap openlist pos n)
	with Not_found -> (* new state *)
	  Dpq.insert openlist n;
	  Htable.add nodes state n in
  let backtrack_count = ref 0 in

  let rec expand_best () =
    (*if you run out of memory, purge open and closed.  Then restart
      using head of open.*)
    if (out_of_memory i) then 
      (
	backtrack_count := !backtrack_count + 1;
	let best = Dpq.extract_first openlist in
	  
	  Dpq.clear openlist;
	  Htable.clear nodes;
	  Dpq.insert openlist best;
	  Htable.add nodes (sface.Search_interface.key best) best;
	  
	  Gc.full_major ();
	  expand_best();
      )
    else if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i))
    then
      (record i openlist;
       let n = Dpq.extract_first openlist in
	 setpos n closed_pos;
	 if not (Limit.promising_p i n) then
	   (Limit.incr_prune i;
	    Htable.remove nodes (sface.Search_interface.key n);
	    expand_best ())
	 else if sface.Search_interface.goal_p n then
	   Limit.new_incumbent i (Limit.Incumbent (0.,n))
	 else
	   (let children = sface.Search_interface.node_expand n in
	      Limit.incr_exp i;
	      List.iter consider_child children;
	      Limit.curr_q i (Dpq.count openlist);
	      expand_best ()))
  in
    expand_best ();

    Datafile.write_pairs stdout ["backtracks",(string_of_int !backtrack_count)];

    Limit.results6 i

    

let dups sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wt = Search_args.get_float "Wted_astar.dups" args 0 in
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h wt)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      wf = neg_infinity;
      f = neg_infinity;
      g = 0.;
      depth = 0;
      pos = Dpq.no_position}
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
  Limit.unwrap_sol6 unwrap_sol
    (search_dups
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       search_interface
       ordered_p
       just_f
       setpos getpos)

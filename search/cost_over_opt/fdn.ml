(**

    @author jtd7
    @since 2012-04-17
*)

open Wted_astar


let make_expand root_h expand h wt =
  (** Takes the domain [expand] function and a [h]euristic calculator.
      Needs the [wt] which will be applied to the heuristic.
      Creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) ->
		 let hv = h d in
		   { data = d;
		     wf = g +. hv +. (min (hv /. root_h) 1.) *. wt;
		     f = g +. hv;
		     g = g;
		     depth = n.depth + 1;
		     pos = Dpq.no_position; }) (expand n.data n.g))


let no_record = (fun _ _ -> ())
and closed_pos = -2

let init_lists pred sface setpos =
  let key = sface.Search_interface.key
  and initial = sface.Search_interface.initial in
  let openlist = Dpq.create pred setpos 100 initial
  and nodes = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 100 in
    Dpq.insert openlist initial;
    Htable.add nodes (key initial) initial;
    openlist, nodes


let search_dups ?(record = no_record) sface ordered_p better_p setpos getpos =
  (** [ordered_p] is true if in order or equal, [better_p] is true if
      superior or equal (eg, for comparing duplicates), [key] gives
      state data for detecting duplicates, [setpos] and [getpos] are
      for efficient swapping into the openlist.  This function is
      intended to be a building block for others - it expects an
      [expand] that takes the openlist as the first argument (in case
      [expand] wants to mess with it). *)
  (* openlist reflects ordering.  nodes has all nodes.  pos is [closedpos]
     for nodes on the closed list. *)
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

  let rec expand_best () =
    if (not (Dpq.empty_p openlist)) && (not (Limit.halt_p i))
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
    i.Limit.log i;
    let finc = (match i.Limit.incumbent with
			Limit.Nothing -> 0.
		      | Limit.Incumbent (_,n) -> n.f) in
    let best_f = ref finc in
      Dpq.iter (fun n -> best_f := min !best_f n.f) openlist;
      let post_bound = finc -. !best_f in
	Datafile.write_pairs stdout ["post_bound", string_of_float post_bound];
	Limit.results6 i


let dups sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let ih = sface.Search_interface.h sface.Search_interface.initial in
  let initial =
    { data = sface.Search_interface.initial;
      wf = ih;
      f = ih;
      g = 0.;
      depth = 0;
      pos = Dpq.no_position} in
  let wt = Search_args.get_float "Wted_astar.dups" args 0 in
  let search_interface = Search_interface.make
    ~node_expand:(make_expand initial.wf sface.Search_interface.domain_expand
		    sface.Search_interface.h wt)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    initial
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


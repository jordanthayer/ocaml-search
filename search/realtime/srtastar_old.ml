(**
    SRTA* search algorithm as through the search interface
    Sofia Lemons - February 2010
*)

type 'a node = {
  data : 'a;          (* Data Payload *)
  f : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
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


let g_then_f a b =
  (** expansion ordering predicate. *)
  ((a.g : float) < b.g) ||
  ((a.g = b.g) && (a.f >= b.f))


let just_g a b =
  (** Sorts nodes solely on incurred cost information *)
  (a.g : float) <= b.g


let just_depth a b =
  (** Sorts nodes solely on node depth information *)
  (a.depth : int) <= b.depth


let f_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  ((a.f : float) < b.f) ||
  ((a.f = b.f) && (a.g >= b.g))


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  (a.f : float) <= b.f

let make_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) -> { data = d;
			       f = g +. (h d);
			       g = g;
			       depth = n.depth + 1; })
       (expand n.data n.g))


let make_sface sface =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~h:(wrap sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	f = neg_infinity;
	g = 0.;
	depth = 0; }
      just_f
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)

let no_record = (fun _ _ -> ())

let srta_choose expand key hash equals info goal_p curr table bound dups =
  (** Chooses the action under curr with the best backed-up f-value
      after a search up to the depth bound.*)
  let alpha = ref infinity
  and closedlist = Htable.create hash equals 0 in
  let srta_eval action =
    let openlist = Dblq.create ()
    and d = (action.depth) in
    let action_alpha = ref infinity in
    let consider_child new_node =
      Limit.incr_gen info;
      if dups then
	(let state = key new_node in
	   try
 	     let prev = Htable.find closedlist state in
	       Limit.incr_dups info;
	       let p_node = Dblq.data prev in
		 if ((p_node.g > new_node.g) ||
		       (p_node.f > new_node.f) ||
		       (new_node.depth > p_node.depth)) then
		   (* better path to previous state *)
		   (* can't go on current g or f alone, because
		      update procedure means that heuristic isn't
		      consistent OR admissible *)
		   (* a node with more depth to explore may hit a
		      node with updated h, leading to a higher
		      f-value at depth bound than another copy of
		      the state at lower depth *)
		   (* will never replace any nodes in unit cost
		      domains except duplicates at same depth as each
		      other *)
		   (* remove old version and insert new at front *)
		   Limit.incr_prune info;
		 let replacing_item =
		   (if not (Dblq.in_q prev) then
		      Dblq.push_front_cons openlist new_node
		    else
		      (if (p_node.g > new_node.g) ||
			 (p_node.f > new_node.f) then
			   ignore (Dblq.remove openlist prev);
		       Dblq.push_front_cons openlist new_node)) in
		   Htable.replace closedlist state replacing_item;
	   with Not_found -> (* new state *)
	     let new_item = Dblq.push_front_cons openlist new_node in
	       Htable.replace closedlist state new_item)
      else
	Dblq.push_front openlist new_node
    in

      if Htable.mem table (key action) then
	(Limit.incr_gen info;
	 alpha := (min (action.g +. (Htable.find table (key action))) !alpha);
	 action.g +. (Htable.find table (key action)))
      else
	(consider_child action;
	 (while not (Dblq.is_empty openlist || Limit.halt_p info) do
	    let n = Dblq.pop_front openlist in
	      if (n.f < !alpha) then
		(if (n.depth == (d + bound)) || (goal_p n) then
		   (alpha := n.f;
		    action_alpha := n.f)
		 else
		   (Limit.incr_exp info;
		    List.iter
		      (fun i ->
			 if Htable.mem table (key i) then
			   consider_child
			     {i with f =
				 (max
				    ((Htable.find
					table
					(key i))
				     +.i.g)
				    n.f)}
			 else
			   consider_child
			     {i with f = (max i.f n.f)})
		      (expand n)))
	      else
		Limit.incr_prune info
	  done);
	 !action_alpha)
  in (Wrlist.min_by srta_eval (expand curr))

let update a =
  a *. 1.1

let srta_search sface bound ?(online = false) dups =
  (** Performs a real-time search by doing a depth bounded search
      under each node, backing up f-values and increasing them to prevent
      cycles.*)
  let i = sface.Search_interface.info
  and table = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 0 in
  let ret_func =
    if dups then Limit.results6
    else (fun x -> (fun (s, x2, x3, x4, x5) ->
		      (s, x2, x3, x4, x5, 0)) (Limit.results5 x)) in
  let start = Wrsys.walltime () in
  let report_move curr next =
    if online then Online.output_row start
      (next.g -. curr.g);
    next in
  let rec move curr =
    if (sface.Search_interface.goal_p curr) then
      (Limit.new_incumbent i (Limit.Incumbent (0.,curr));
       ret_func i)
    else if (Limit.halt_p sface.Search_interface.info) then
      ret_func i
    else
      (
	let alpha = (srta_choose sface.Search_interface.node_expand
		       sface.Search_interface.key
		       sface.Search_interface.hash
		       sface.Search_interface.equals
		       sface.Search_interface.info
		       sface.Search_interface.goal_p curr table (bound-1) dups) in
	  Htable.replace table
	    (sface.Search_interface.key curr)
	    (update ((snd alpha)-.curr.g));
	  (*Printf.printf "current f: %f\n" curr.f;*)
	  Limit.incr_exp sface.Search_interface.info;
	  move (report_move curr (fst alpha))
      )
  in
    move sface.Search_interface.initial

let no_dups sface args =
  (** Performs an SRTA* search from the initial state to a goal,
      for domains with no duplicates. *)
  let bound = Search_args.get_int "Srtastar.no_dups" args 0 in
  let search_interface = make_sface sface in
    Limit.unwrap_sol6_into_5 unwrap_sol
      (srta_search
	 search_interface
	 bound
	 false)

let dups sface args =
  (** Performs an SRTA* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let bound = Search_args.get_int "Srtastar.dups" args 0 in
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (srta_search
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 bound ~online:true
	 true)

let online_no_dups sface args =
  (** Performs an SRTA* search from the initial state to a goal,
      for domains with no duplicates. *)
  let bound = Search_args.get_int "Srtastar.no_dups" args 0 in
  let search_interface = make_sface sface in
    Online.output_col_hdr ();
    Limit.unwrap_sol6_into_5 unwrap_sol
      (srta_search
	 search_interface
	 bound
	 false)

let online_dups sface args =
  (** Performs an SRTA* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let bound = Search_args.get_int "Srtastar.dups" args 0 in
  let search_interface = make_sface sface in
    Online.output_col_hdr ();
    Limit.unwrap_sol6 unwrap_sol
      (srta_search
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 bound ~online:true
	 true)

(* Drop dups refers to whether duplicates are dropped in the
   action-selection search. Duplicates have to be reconsidered in the
   top-level search. *)

(* EOF *)

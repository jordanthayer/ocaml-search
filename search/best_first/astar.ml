(**
    A* search algorithm as through the search interface
    Jordan Thayer - June 2009
*)

type 'a node = {
  data : 'a;          (* Data Payload *)
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
    | Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let f_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  ((a.f : float) < b.f) ||
    ((a.f = b.f) && (a.g >= b.g))


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  (a.f : float) <= b.f


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.filter (fun n -> n.f < infinity)
     (List.map (fun (d, g) -> { data = d;
			       f = g +. (h d);
			       g = g;
			       depth = n.depth + 1;
			       pos = Dpq.no_position; }) (expand n.data n.g)))


let make_sface sface =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      {data = sface.Search_interface.initial;
       f = sface.Search_interface.h sface.Search_interface.initial;
       g = 0.;
       depth = 0;
       pos = Dpq.no_position;}
      just_f
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let no_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Astar.no_dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 f_then_g
	 just_f)


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Astar.dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 f_then_g
	 just_f
	 setpos
	 getpos)


let online_no_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Astar.no_dups" args;
  let search_interface = make_sface sface in
  let start = Wrsys.walltime () in
  let report_moves x = (match x with
			    (None, _, _, _, _) -> ()
			  | (Some (_, g), _, _, _, _) -> Online.output_row
			      start
				g);
    x in
    Online.output_col_hdr ();
    (report_moves (Limit.unwrap_sol5 unwrap_sol
       (Best_first.search
	  search_interface
	  f_then_g
	  just_f)))

let online_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Astar.dups" args;
  let search_interface = make_sface sface in
  let start = Wrsys.walltime () in
  let report_moves x = (match x with
			    (None, _, _, _, _, _) -> Online.output_row
			      (Sys.time ())
			      infinity
			  | (Some (_, g), _, _, _, _, _) -> Online.output_row
			      start
				g);
    x in
    Online.output_col_hdr ();
    (report_moves (Limit.unwrap_sol6 unwrap_sol
		     (Best_first.search_dups
			(* must have g=0 as base for others, and
			   f<others to prevent re-opening *)
			search_interface
			f_then_g
			just_f
			setpos
			getpos)))


let no_dups_silent sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Astar.no_dups" args;
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~halt_on:sface.Search_interface.halt_on
    sface.Search_interface.domain
    {data = sface.Search_interface.initial;
     f = neg_infinity;
     g = 0.;
     depth = 0;
     pos = Dpq.no_position;}
    just_f
    (fun i -> ()) in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 f_then_g
	 just_f)

let dups_silent sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with duplicates. *)
  Search_args.is_empty "Astar.no_dups" args;
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~halt_on:sface.Search_interface.halt_on
    sface.Search_interface.domain
    {data = sface.Search_interface.initial;
     f = neg_infinity;
     g = 0.;
     depth = 0;
     pos = Dpq.no_position;}
    just_f
    (fun i -> ()) in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 search_interface
	 f_then_g
	 just_f
	 setpos
	 getpos)


let seeded_dups_silent i_children sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Astar.dups" args;
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      {data = sface.Search_interface.initial;
       f = neg_infinity;
       g = 0.;
       depth = 0;
       pos = Dpq.no_position;}
      just_f
      (fun i -> ()) in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups_seeded
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 (List.map (fun data -> {data = data;
				 f = 0.;
				 g = 0.;
				 depth = 0;
				 pos = Dpq.no_position}) i_children)
	 search_interface
	 f_then_g
	 just_f
	 setpos
	 getpos)

(* Drop dups would have no use for A*.  Nodes are optimal on first
   encounter assuming an admissible heuristic. *)


let b_no_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Astar.b_no_dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol5 unwrap_sol
      (B.search
	 search_interface
	 f_then_g
	 (fun n -> n.f)
	 (fun n -> n.g)
	 setpos
	 getpos)


let b_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Astar.b_dups" args;
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (B.search_dups
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 f_then_g
	 just_f
	 (fun n -> n.f)
	 (fun n -> n.g)
	 getpos
	 setpos)


let k_no_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let k = Search_args.get_int "Astar.k_no_dups" args 0 in
  let search_interface = make_sface sface in
    Limit.unwrap_sol5 unwrap_sol
      (K_best_first.search
	 k
	 search_interface
	 f_then_g
	 just_f)


let k_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Verb.pe Verb.always "kbfs\n";
  let k = Search_args.get_int "Astar.k_dups" args 0 in
  let search_interface = make_sface sface in
    Limit.unwrap_sol6 unwrap_sol
      (K_best_first.search_dups
	 k
	 search_interface
	 f_then_g
	 just_f
	 setpos
	 getpos)

(* EOF *)

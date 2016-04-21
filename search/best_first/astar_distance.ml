(* Astar which finds shortest possible paths *)

open Astar


let make_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) -> let dep = n.depth + 1 in
		 { data = d;
		   f = (float_of_int dep) +. (h d);
		   g = g;
		   depth = dep;
		   pos = Dpq.no_position; }) (expand n.data n.g))

let no_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Astar_distance.dups" args;
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.d)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      f = neg_infinity;
      g = 0.;
      depth = 0;
      pos = Dpq.no_position;}
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))
  in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 f_then_g
	 just_f)


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Astar_distance.dups" args;
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.d)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~halt_on:sface.Search_interface.halt_on
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      f = neg_infinity;
      g = 0.;
      depth = 0;
      pos = Dpq.no_position;}
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
  Limit.unwrap_sol6 unwrap_sol
    (Best_first.search_dups
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       search_interface
       f_then_g
       just_f
       setpos
       getpos)

(* Drop dups would have no use for A*.  Nodes are optimal on first
   encounter assuming an admissible heuristic. *)

(* EOF *)

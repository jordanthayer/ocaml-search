(** K-weighted A* *)
open Wted_astar


let no_dups sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let wt = Search_args.get_float "K_weighted_astar.no_dups" args 0
  and k = Search_args.get_int "K_weighted_astar.no_dups" args 1 in
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h wt)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
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
       (wrap sface.Search_interface.get_sol_length))
  in
    Limit.unwrap_sol5 unwrap_sol
      (K_best_first.search
	 k
	 search_interface
	 ordered_p
	 just_f)


let dups sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wt = Search_args.get_float "K_weighted_astar.dups" args 0
  and k = Search_args.get_int "K_weighted_astar.dups" args 1 in
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
    (K_best_first.search_dups
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       k
       search_interface
       ordered_p
       just_f
       setpos getpos)

(* EOF *)

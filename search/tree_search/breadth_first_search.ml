(**
   Breadth first search algorithm as through the search interface
   Jordan Thayer - July 2009 *)

type 'a node = {
  data : 'a;
  g : float;
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


let better_p a b =
  (** Sorts nodes solely on total cost information *)
  a.g <= b.g


let make_expand expand =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) -> { data = d;
			       g = g;}) (expand n.data n.g))


let no_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Breadth_first_search.no_dups" args;
  let search_interface =
    (Search_interface.make
       ~node_expand:(make_expand sface.Search_interface.domain_expand)
       ~goal_p:(wrap sface.Search_interface.goal_p)
       ~halt_on:sface.Search_interface.halt_on
       ~hash:sface.Search_interface.hash
       ~equals:sface.Search_interface.equals
       sface.Search_interface.domain
       {data = sface.Search_interface.initial;
	g = 0.;})
      better_p
      (Limit.make_default_logger (fun n -> n.g)
	 (wrap sface.Search_interface.get_sol_length))
  in
      Limit.unwrap_sol5 unwrap_sol
      (Breadth_first.no_dups
	 search_interface
	 better_p)


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Breadth_first_search.dups" args;
  let search_interface =
    (Search_interface.make
       ~node_expand:(make_expand sface.Search_interface.domain_expand)
       ~goal_p:(wrap sface.Search_interface.goal_p)
       ~halt_on:sface.Search_interface.halt_on
       ~hash:sface.Search_interface.hash
       ~equals:sface.Search_interface.equals
       sface.Search_interface.domain
       {data = sface.Search_interface.initial;
	g = 0.;})
      better_p
      (Limit.make_default_logger (fun n -> n.g)
	 (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Breadth_first.dups
	 search_interface
	 better_p)


let drop_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Breadth_first_search.drop_dups" args;
  let search_interface =
    (Search_interface.make
       ~node_expand:(make_expand sface.Search_interface.domain_expand)
       ~goal_p:(wrap sface.Search_interface.goal_p)
       ~halt_on:sface.Search_interface.halt_on
       ~hash:sface.Search_interface.hash
       ~equals:sface.Search_interface.equals
       sface.Search_interface.domain
       {data = sface.Search_interface.initial;
	g = 0.;})
      better_p
      (Limit.make_default_logger (fun n -> n.g)
	 (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Breadth_first.drop_dups
	 search_interface
	 better_p)


(* EOF *)

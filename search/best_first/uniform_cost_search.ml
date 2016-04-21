(**
    Uniform Cost Search Algorithm as through the search interface
    Jordan Thayer - July 2009
*)

type 'a node = {
  data : 'a;          (* Data Payload *)
  g : float;          (* Cost of reaching a node *)
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


let ordered a b =
  a.g <= b.g


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
     List.map (fun (d, g) -> { data = d;
			       g = g;
			       pos = Dpq.no_position; }) (expand n.data n.g))


let make_avg_bf_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  let samples = ref 0
  and kids = ref 0 in
  (fun n ->
     samples := !samples + 1;
     let children =
       List.map (fun (d, g) -> { data = d;
				 g = g;
				 pos = Dpq.no_position; }) (expand n.data n.g)
     in kids := !kids + (List.length children);
       Verb.pe Verb.always "avg. bfactor\t%f\n%!"
	 ((float !kids) /. (float !samples));
       children)


let no_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Uniform_cost_search.no_dups" args;
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      pos = Dpq.no_position;}
    ordered
    (Limit.make_default_logger (fun n -> n.g)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 ordered
	 ordered)


let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Uniform_cost_search_dups" args;
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~key:(wrap sface.Search_interface.key)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      pos = Dpq.no_position;}
    ordered
    (Limit.make_default_logger (fun n -> n.g)
       (wrap sface.Search_interface.get_sol_length)) in
  Limit.unwrap_sol6 unwrap_sol
    (Best_first.search_dups
       search_interface
       ordered
       ordered
       setpos
       getpos)


let dups_abf sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Uniform_cost_search_dups" args;
  let search_interface =  Search_interface.make
    ~node_expand:(make_avg_bf_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~key:(wrap sface.Search_interface.key)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      pos = Dpq.no_position;}
    ordered
    (Limit.make_default_logger (fun n -> n.g)
       (wrap sface.Search_interface.get_sol_length)) in
  Limit.unwrap_sol6 unwrap_sol
    (Best_first.search_dups
       search_interface
       ordered
       ordered
       setpos
       getpos)


let no_dups_silent sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Uniform_cost_search.no_dups" args;
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      pos = Dpq.no_position;}
    ordered
    (fun i -> ()) in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 ordered
	 ordered)


let dups_silent sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Uniform_cost_search_dups" args;
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~key:(wrap sface.Search_interface.key)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      pos = Dpq.no_position;}
    ordered
    (fun i -> ()) in
  Limit.unwrap_sol6 unwrap_sol
    (Best_first.search_dups
       search_interface
       ordered
       ordered
       setpos
       getpos)


(* EOF *)

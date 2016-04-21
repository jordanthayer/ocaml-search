(**
    Greedy search algorithm as through the search interface
    Jordan Thayer - July 2009
*)

type 'a node = {
  data : 'a;          (* Data Payload *)
  h : float;          (* Heuristic at node*)
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


let h_then_g a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  (a.h < b.h) ||
  ((a.h = b.h) && (a.g >= b.g))


let just_g a b =
  (** Sorts nodes solely on total cost information *)
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
			       h = (h d);
			       g = g;
			       depth = n.depth + 1;
			       pos = Dpq.no_position; }) (expand n.data n.g))


let no_dups sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Greedy.no_dups" args;
  let h = (try (Heuristic.default_fixed
		  sface.Search_interface.heuristics).Heuristic.heuristic
	   with _ -> (Verb.pe Verb.always "Heuristics not set for domain!\n%!";
		      sface.Search_interface.h)) in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	h = neg_infinity;
	g = 0.;
	depth = 0;
	pos = Dpq.no_position;}
      just_g
      (Limit.make_default_logger (fun n -> n.g)
	 (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 h_then_g
	 just_g)


let dups sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Greedy.dups" args;
  let h = (try (Heuristic.default_fixed
		  sface.Search_interface.heuristics).Heuristic.heuristic
	   with _ -> (Verb.pe Verb.always "Heuristics not set for domain!\n%!";
		      sface.Search_interface.h)) in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	h = neg_infinity;
	g = 0.;
	depth = 0;
	pos = Dpq.no_position;}
      just_g
      (Limit.make_default_logger (fun n -> n.g)
	 (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 h_then_g
	 just_g
	 setpos
	 getpos)


let drop_dups sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains where duplicates are frequently encountered.
      When duplicates are seen, they are ignored immediately. *)
  Search_args.is_empty "Greedy.drop_dups" args;
  let h = (try (Heuristic.default_fixed
		  sface.Search_interface.heuristics).Heuristic.heuristic
	   with _ -> (Verb.pe Verb.always "Heuristics not set for domain!\n%!";
		      sface.Search_interface.h)) in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	h = neg_infinity;
	g = 0.;
	depth = 0;
	pos = Dpq.no_position;}
      just_g
      (Limit.make_default_logger (fun n -> n.g)
	 (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_drop_dups
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 h_then_g
	 just_g
	 setpos
	 getpos)


(* EOF *)

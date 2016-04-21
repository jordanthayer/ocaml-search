(**

    @author jtd7
    @since 2010-09-29
   Greedy search using the round robin technique proposed by Gabi Roeger
*)

type 'a node = {
  data : 'a;
  h : float array;
  g : float;
  pos : int array;
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


let h_then_g i a b =
  (** expansion ordering predicate, and also works for ordering duplicates
      assuming that h is the same for both
      (hence f will be lower when g is lower). *)
  a.h.(i) < b.h.(i) ||
    ((a.h.(i) = b.h.(i)) && (a.g >= b.g))


let just_g a b =
  (** Sorts nodes solely on total cost information *)
  a.g <= b.g

let setpos i n pos =
  (** Sets the location of a node, used by dpq's *)
  n.pos.(i) <- pos


let getpos i n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos.(i)


let make_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  let num_h = Array.length h in
  (fun n ->
     List.map (fun (d, g) -> { data = d;
			       h = Array.map (fun h -> h d) h;
			       g = g;
			       pos = Array.create num_h Dpq.no_position; })
       (expand n.data n.g))


let drop sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Greedy.no_dups" args;
  let l = (List.map (fun h -> h.Heuristic.heuristic)
	     sface.Search_interface.heuristics.Heuristic.fixed) in
  let h_array =  (if (List.length l > 0) then Array.of_list l
		  else [| sface.Search_interface.h;
			  sface.Search_interface.d; |]) in
  let hleng = Array.length h_array in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      h_array)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	h = Array.create hleng neg_infinity;
	g = 0.;
	pos = Array.create hleng Dpq.no_position;}
      just_g
      (Limit.make_default_logger (fun n -> n.g)
	 (wrap sface.Search_interface.get_sol_length)) in
    Verb.pe Verb.toplvl "%i queues\n%!" hleng;
    Limit.unwrap_sol6 unwrap_sol
      (Round_robin_best_first.search_dd
	 search_interface
	 hleng
	 h_then_g
	 setpos
	 getpos
	 just_g)

let dups sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Greedy.no_dups" args;
  let l = (List.map (fun h -> h.Heuristic.heuristic)
	     sface.Search_interface.heuristics.Heuristic.fixed) in
  let h_array =  (if (List.length l > 0) then Array.of_list l
		  else [| sface.Search_interface.h;
			  sface.Search_interface.d; |]) in
  let hleng = Array.length h_array in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      h_array)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	h = Array.create hleng neg_infinity;
	g = 0.;
	pos = Array.create hleng Dpq.no_position;}
      just_g
      (Limit.make_default_logger (fun n -> n.g)
	 (wrap sface.Search_interface.get_sol_length)) in
    Verb.pe Verb.toplvl "%i queues\n%!" hleng;
    Limit.unwrap_sol6 unwrap_sol
      (Round_robin_best_first.search_dups
	 search_interface
	 hleng
	 h_then_g
	 setpos
	 getpos
	 just_g)

let no_dups sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains with no duplicates. *)
  Search_args.is_empty "Greedy.no_dups" args;
  let l = (List.map (fun h -> h.Heuristic.heuristic)
	     sface.Search_interface.heuristics.Heuristic.fixed) in
  let h_array =  (if (List.length l > 0) then Array.of_list l
		  else [| sface.Search_interface.h;
			  sface.Search_interface.d; |]) in
  let hleng = Array.length h_array in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      h_array)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	h = Array.create hleng neg_infinity;
	g = 0.;
	pos = Array.create hleng Dpq.no_position;}
      just_g
      (Limit.make_default_logger (fun n -> n.g)
	 (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol5 unwrap_sol
      (Round_robin_best_first.search
	 search_interface
	 hleng
	 h_then_g
	 setpos
	 getpos
	 just_g)


(* EOF *)

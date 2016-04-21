(** Full A* is a version of A* that does not stop when it finds its
    solution, instead it continues until the final f-layer is exhausted.
    This algorithm is really only to be used to collect worst-case
    information on problem instances.

    This file is based on anytime_astar.ml

    @author eaburns
    @since 2010-05-28
*)


type 'a node = {
  data : 'a;          (* Data Payload *)
  f : float;          (* Total cost of a node*)
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


let ordered_p a b = (a.f < b.f) || ((a.f = b.f) && (a.g >= b.g))
  (** Ordered predicate used for search.  Compares f', then f, then g.
      true if a is better than b. *)


let just_f a b = a.f <= b.f
  (** Sorts nodes solely on total cost information *)


let better_p a b = a.f < b.f
  (** Compare the quality of two nodes based on f.  This is used for
      pruning.  Since we want to expand the entire final f-layer this
      prunes only when b.f is worse than a.f. *)


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos


let make_expand expand h =
  (** Takes the domain [expand] function and a [h]euristic calculator.
      Creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) ->
		 let hv = h d in
		   { data = d;
		     f = g +. hv;
		     g = g;
		     pos = Dpq.no_position; }) (expand n.data n.g))



let make_iface sface =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~key:(wrap sface.Search_interface.key)
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	f = neg_infinity;
	g = 0.;
	pos = Dpq.no_position;}
      better_p			(* used to create Limit.promising_p *)
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let no_dups sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains with no duplicates. *)
  Limit.unwrap_sol5 unwrap_sol
    (Continued_search.no_dups
       (make_iface sface)
       ordered_p)


let dups sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Limit.unwrap_sol6 unwrap_sol
    (Continued_search.dups
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       (make_iface sface)
       just_f (* Used to test if there is a better path to a duplicate. *)
       ordered_p		     (* Ordering for the open list. *)
       setpos getpos)


(* EOF *)

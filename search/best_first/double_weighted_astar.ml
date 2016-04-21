(**

    @author jtd7
    @since 2011-02-17
*)

type node_type =
  | Nearest
  | Cheapest

type 'a node = {
  data : 'a;          (* Data Payload *)
  wf: float;          (* Search order w/ a weighted heuristic value *)
  f : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  depth : int;        (* Depth of node in tree.  Root @ 0 *)
  mutable pos : int;  (* Position info for dpq *)
  t : node_type;
}

let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let wrap_key f =
  (fun n -> f n.data, n.t)


let wrap_hash hash (key, typ) = hash key


let wrap_eq eq (key1,typ1) (key2,typ2) =
  typ1 = typ2 && (eq key1 key2)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let ordered_p a b =
  (** Ordered predicate used for search.  Compares f', then f, then g.
      true if a is better than b.
  *)
  (a.wf < b.wf) ||
  ((a.wf = b.wf) &&
   ((a.f < b.f) ||
    ((a.f = b.f) &&
     (a.g >= b.g))))


let just_f a b =
  (** Sorts nodes solely on total cost information *)
  a.f <= b.f


let setpos n i =
  (** Sets the location of a node, used by dpq's *)
  n.pos <- i


let getpos n =
  (** Returns the position of the node in its dpq.
      Useful for swapping nodes around on the open list *)
  n.pos

let make_expand expand h_near h_cheap wt =
  (fun n ->
     List.fold_left (fun accum (d, g) ->
		       let hv_near = h_near d
		       and hv_cheap = h_cheap d
		       and depth' = n.depth + 1 in
			 { data = d;
			   wf = g +. wt *.hv_near;
			   f = g +. hv_near;
			   g = g;
			   depth = depth';
			   pos = Dpq.no_position;
			   t = Nearest; }::
			   { data = d;
			     wf = g +. wt *. hv_cheap;
			     f = g +. hv_cheap;
			     g = g;
			     depth = depth';
			     pos = Dpq.no_position;
			     t = Cheapest; } :: accum) []
       (expand n.data n.g))



let no_dups sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let wt = Search_args.get_float "Wted_astar.no_dups" args 0 in
  let hnear = (List.hd
		 (Heuristic.get_fixed sface.Search_interface.heuristics
		    [Heuristic.Admissible; Heuristic.Cost;
		     Heuristic.Forwards; Heuristic.Nearest])).Heuristic.heuristic
  and hcheap = (List.hd
		  (Heuristic.get_fixed sface.Search_interface.heuristics
		     [Heuristic.Admissible; Heuristic.Cost;
		      Heuristic.Forwards;
		      Heuristic.Cheapest])).Heuristic.heuristic in
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    hnear hcheap wt)
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
      pos = Dpq.no_position;
      t = Cheapest;}
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))
  in
    Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 ordered_p
	 just_f)


let dups sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let wt = Search_args.get_float "Wted_astar.no_dups" args 0 in
  let hnear = (Heuristic.get_fixed sface.Search_interface.heuristics
		 [Heuristic.Admissible; Heuristic.Cost;
		  Heuristic.Forwards; Heuristic.Nearest])
  and hcheap = (Heuristic.get_fixed sface.Search_interface.heuristics
		     [Heuristic.Admissible; Heuristic.Cost;
		      Heuristic.Forwards;
		      Heuristic.Cheapest]) in
    assert (hnear <> []);
    assert (hcheap <> []);
    let hnear = (List.hd hnear).Heuristic.heuristic
    and hcheap = (List.hd hcheap).Heuristic.heuristic in
  let key = wrap_key sface.Search_interface.key in
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    hnear hcheap wt)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~key:key
    ~hash:(wrap_hash sface.Search_interface.hash)
    ~equals:(wrap_eq sface.Search_interface.equals)
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      wf = neg_infinity;
      f = neg_infinity;
      g = 0.;
      depth = 0;
      pos = Dpq.no_position;
      t = Cheapest; }
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))
  in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 search_interface
	 ordered_p
	 just_f
	 setpos
	 getpos)


let drop sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let wt = Search_args.get_float "Wted_astar.no_dups" args 0 in
  let hnear = (List.hd
		 (Heuristic.get_fixed sface.Search_interface.heuristics
		    [Heuristic.Admissible; Heuristic.Cost;
		     Heuristic.Forwards; Heuristic.Nearest])).Heuristic.heuristic
  and hcheap = (List.hd
		  (Heuristic.get_fixed sface.Search_interface.heuristics
		     [Heuristic.Admissible; Heuristic.Cost;
		      Heuristic.Forwards;
		      Heuristic.Cheapest])).Heuristic.heuristic in
  let key = wrap_key sface.Search_interface.key in
  let search_interface = Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    hnear hcheap wt)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~key:key
    ~hash:(wrap_hash sface.Search_interface.hash)
    ~equals:(wrap_eq sface.Search_interface.equals)
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      wf = neg_infinity;
      f = neg_infinity;
      g = 0.;
      depth = 0;
      pos = Dpq.no_position;
      t = Cheapest; }
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))
  in
    Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_drop_dups
	 search_interface
	 ordered_p
	 just_f
	 setpos
	 getpos)



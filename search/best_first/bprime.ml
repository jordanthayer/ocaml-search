(** B' is an extension of the B algorithm which does bidirectionl pathmax
    Based on Felner et al A* Search with Inconsistent Heuristics *)


type 'a node = {
  data : 'a;          (* Data Payload *)
  mutable f : float;          (* Total cost of a node*)
  g : float;          (* Cost of reaching a node *)
  mutable h : float;
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


let make_expand_bidirectional expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes.
      Additionally, does a single step of bidirection pathmax,
      so it can only be run on domains with invertible operators.*)
  (fun n ->
     let children, pmax_tups =
       List.fold_left
	 (fun (c_accum,pm_accum) (d, g) ->
	    let hn = h d in
	      ({ data = d;
		f = g +. hn;
		g = g;
		h = hn;
		pos = Dpq.no_position; }::c_accum),
	    ((g -. n.g), hn)::pm_accum) ([],[]) (expand n.data n.g) in
     let updated = ref false in
       (* Try to find a better h for the parent *)
       List.iter
	 (fun (t_cost,c_h) ->
	    let possible_update = c_h -. t_cost in
	      if possible_update > n.h
	      then (updated := true;
		    n.h <- possible_update;
		    n.f <- (n.g +. possible_update))) pmax_tups;
       if !updated
	 (* Parent was updated, propegate to kids *)
       then (List.iter2
	       (fun (t_cost, c_h) c ->
		  let possible_update = n.h -. t_cost in
		    if possible_update > c_h
		    then (c.h <- possible_update;
			  c.f <- c.g +. possible_update))
	       pmax_tups children);
       children)


let make_expand_directed expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes.
      Additionally, does a single step of bidirection pathmax,
      so it can only be run on domains with invertible operators.*)
  (fun n ->
     List.map
       (fun (d, g) ->
	  let hn = h d in
	  let my_h = Math.fmax hn (n.h -. (g -. n.g)) in
	    { data = d;
	      f = g +. my_h;
	      g = g;
	      h = my_h;
	      pos = Dpq.no_position; }) (expand n.data n.g))


let make_sface exp sface =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(exp sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	f = neg_infinity;
	h = neg_infinity;
	g = 0.;
	pos = Dpq.no_position;}
      just_f
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let no_dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Bprime.no_dups" args;
  let search_interface = make_sface make_expand_directed sface in
    Limit.unwrap_sol5 unwrap_sol
      (B.search
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 f_then_g
	 (fun n -> n.f)
	 (fun n -> n.g)
	 setpos
	 getpos)



let dups sface args =
  (** Performs an A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  Search_args.is_empty "Bprime.dups" args;
  let search_interface = make_sface make_expand_bidirectional sface in
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


(* EOF *)

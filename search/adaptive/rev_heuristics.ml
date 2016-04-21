(** Reverse Heuristic Search.  Searches which apply two sets of heuristics,
    those looking towards the goal and those looking towards the root.  This is
    used mostly in heuristic correction, assuming that depth, g, and what they
    telly us about the backwards looking heruistics apply to the forward
    looking heuristics as well. *)

type 'a node = {
  mutable est_f : float;
  h : float;
  d : float;
  rev_h : float;
  rev_d : float;
  g : float;
  depth : int;
  mutable q_pos : int;   (* for Dpq *)
  data : 'a;
}


let setpos n i =
  (** Updates the [q_pos] of node [n], setting it to [i] *)
  n.q_pos <- i


let getpos n =
  (** returns the current [q_pos] of node [n] *)
  n.q_pos


let wrap f =
  (** Wraps a function [f] which works on domain data and makes it so
      that it can be applied to nodes *)
  (fun n -> f n.data)


let est_f_then_d_then_g a b =
  (** sorts nodes in order of estimated f, breaking ties in favor of
      high g values *)
  (a.est_f < b.est_f) ||  (* sort by fhat *)
    ((a.est_f = b.est_f) &&
       a.g > b.g)

let better_p a b =
  (** Determines which of the nodes represents a better solution *)
  (a.g +. a.h) <= (b.g +. b.h)


let unwrap_sol s =
  (** Decomposes the solution [s] into a form which the domains are expecting
      when doing validation *)
  match s with
      Limit.Incumbent (q, n) -> Some (n.data, n.g)
    | _ -> None


let wrap_incumbent i =
  (** Wraps an incumbent solution [i] and returns a Limit.info based on it *)
  match i with
      None -> Limit.Nothing
    | Some (n) -> Limit.Incumbent (0., n)


let make_expand expand hd rev_hd timer calc_h_data f_calc =
  (** [expand] is the domain expand
      [hd] is a cost and distance estimator
      [timer] returns true every so often, to tell the openlist to resort
      [calc_h_data] takes the parent, the best child, and all children
      in order to make a better h estimator
      [f_calc] uses the estimated h values to calculate the bounded f
      estimates *)
  (fun n ->
     let best_f = ref infinity
     and best_child = ref n
     and reorder = timer() in
     let children = (List.map (fun (s, g) ->
				 let h,d = hd s
				 and rh,rd = rev_hd s in
				 let f = g +. h in
				 let c =
				   { est_f = f;
				     h = h;
				     d = d;
				     rev_h = rh;
				     rev_d = rd;
				     depth = n.depth + 1;
				     g = g;
				     q_pos = Dpq.no_position;
				     data = s;} in
				   if  f < !best_f then
				     (best_child := c;
				      best_f := f);
				   c)
		       (expand n.data n.g))
     in
       if not ((List.length children) = 0)
       then
	 (calc_h_data n !best_child children;
	  List.iter (fun c -> c.est_f <- f_calc c) children);
       reorder, children)


let make_updater f_calc =
  (** Updates the estimated f values of all nodes in a given dpq *)
  (fun dpq ->
     Dpq.iter (fun n -> n.est_f <- f_calc n) dpq)


(************************ Search Callers, internal only **********************)

let no_dups sface bound timer h_calc f_calc =
  (** Performs a search in domains where there are no duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  let search_interface = Search_interface.make
    ~resort_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd sface.Search_interface.rev_hd
		    timer h_calc f_calc)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:(sface.Search_interface.halt_on)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { est_f = neg_infinity;
      h = neg_infinity;
      d = neg_infinity;
      rev_h = 0.;
      rev_d = 0.;
      g = 0.;
      depth = 0;
      q_pos = Dpq.no_position;
      data = sface.Search_interface.initial;}
    better_p
    (Limit.make_default_logger (fun n -> n.g +. n.h)
       (wrap sface.Search_interface.get_sol_length))
  in
    Limit.unwrap_sol5 unwrap_sol
      (Reorderable_best_first.search
	 search_interface
	 est_f_then_d_then_g
	 better_p
	 (make_updater f_calc))


let dups sface bound timer h_calc f_calc =
  (** Performs a search in domains where there are duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  let search_interface = Search_interface.make
    ~resort_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.hd sface.Search_interface.rev_hd
		      timer h_calc f_calc)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:(sface.Search_interface.halt_on)
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { est_f = neg_infinity;
      h = neg_infinity;
      d = neg_infinity;
      rev_h = 0.;
      rev_d = 0.;
      g = 0.;
      depth = 0;
      q_pos = Dpq.no_position;
      data = sface.Search_interface.initial;}
    better_p
    (Limit.make_default_logger (fun n -> n.g +. n.h)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Reorderable_best_first.search_dups
	 search_interface
	 est_f_then_d_then_g
	 better_p
	 setpos
	 getpos
	 (make_updater f_calc))


let drop sface bound timer h_calc f_calc =
  (** Performs a search in domains where there are duplicates.
      [sface] is the domain's search interface
      [bound] is the desired quality bound
      [timer] is a timer from Timers.ml
      [h_calc] takes the parent, best child, and all children, returns unit
      [f_calc] takes a node and returns a float *)
  let search_interface = Search_interface.make
    ~resort_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.hd sface.Search_interface.rev_hd
		      timer h_calc f_calc)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:(sface.Search_interface.halt_on)
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { est_f = neg_infinity;
      h = neg_infinity;
      d = neg_infinity;
      rev_h = 0.;
      rev_d = 0.;
      g = 0.;
      depth = 0;
      q_pos = Dpq.no_position;
      data = sface.Search_interface.initial;}
    better_p
    (Limit.make_default_logger (fun n -> n.g +. n.h)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Reorderable_best_first.search_drop_dups
	 search_interface
	 est_f_then_d_then_g
	 better_p
	 setpos
	 getpos
	 (make_updater f_calc))

(* EOF *)

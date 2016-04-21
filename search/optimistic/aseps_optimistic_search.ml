(** Calls in to A* eps optimistc in order to perform searches for the
    domains *)


type 'a node = {
  f : float;
  g : float;
  d : float;
  mutable q_pos : int;
  mutable c_pos : int;
  data : 'a;
}


let just_f a b =
  (** quality ordering predicate.  distinctions between nodes with the same
    f are made by the convenience ordering predicate. *)
  a.f <= b.f


let d_then_f_then_g a b =
  (** convenience ordering predicate *)
  (a.d < b.d) ||
  ((a.d = b.d) && ((a.f < b.f) ||
		   ((a.f = b.f) && (a.g >= b.g))))


let get_q_pos n =
  (** returns the q_pos of node [n] *)
  n.q_pos


let set_q_pos n pos =
  (** sets the q_pos of node [n] *)
  n.q_pos <- pos


let get_c_pos n =
  (** returns the c_pos of node [n] *)
  n.c_pos


let set_c_pos n pos =
  (** sets the c_pos of node [n] to [pos] *)
  n.c_pos <- pos


let make_close_enough_p weight =
  assert (weight >= 1.);
  (fun a b ->
     (** is b good enough compared to the benchmark a? *)
     b.f <= (a.f *. weight))


let unwrap_sol n =
  (** munges the solution into something the domains can use *)
  match n with
      Limit.Incumbent (q,n) -> Some (n.data, n.g)
    | _ -> None


let wrap_incumbent i =
  (** Takes the incumbent and returns a limit based on it. *)
  match i with
    None -> Limit.Nothing
  | Some (n, g) -> Limit.Incumbent (0., { f = g;
					  g = g;
					  d = 0.;
					  q_pos = Dpq.no_position;
					  c_pos = Dpq.no_position;
					  data = n;
					})


let make_root initial =
  (** builds an initial search node based on the domain root [initial]*)
  { f = neg_infinity;
    g = 0.;
    d = neg_infinity;
    q_pos = Dpq.no_position;
    c_pos = Dpq.no_position;
    data = initial; }


let make_expand expand hd =
  (** Converts the domains [expand] function into a suitable search expand
      function.  Needs a cost and distance estimator provided by [hd] *)
  (fun n ->
     List.map (fun (n, g) ->
		 let h, d = hd n in
		   { f = g +. h;
		     g = g;
		     d = d;
		     q_pos = Dpq.no_position;
		     c_pos = Dpq.no_position;
		     data = n; })
     (expand n.data n.g))



let get_node bound fq geq i =
  (** Returns the next node for the search to expand.  Decision is based upon
      required [bound] and the nodes at the front of [fq] and [geq].  Needs
      the limit [i] in order to add new incumbents and decide on when to stop
  *)
  let fn = Dpq.peek_first fq
  and incumbent = i.Limit.incumbent in
    match incumbent with
	Limit.Nothing ->  Geq.peek_best geq, true
      | Limit.Incumbent(qual,inc) ->
	  if (fn.f *. bound) >= inc.f
	  then fn,false
	  else
	    (let fpn = Geq.peek_best geq in
	       if (fn.f *. bound) < inc.f
	       then fpn,true
	       else fn,true)


let wrap fn =
  (** Takes a function [fn] designed for use on domain nodes and makes it
      apply to search nodes *)
  (fun n -> fn n.data)



(********************** Search ********************************)

let no_dups sface args =
  (** A* eps optimistic search for domains with few / no duplicate states.
      Requires the search interface [sface] a quality [bound] and a level of
      [optimism] which tells us how aggressive to be in the a* eps phase of the
      search *)
  let bound = Search_args.get_float
    "Aseps_optimistic_search.no_dups" args 0
  and optimism = Search_args.get_float
    "Aseps_optimistic_search.no_dups" args 1 in
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd)
    ~key:(wrap sface.Search_interface.key)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_root sface.Search_interface.initial)
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))
  in
    Limit.unwrap_sol5 unwrap_sol
      (Aseps_optimistic.no_dups
	 search_interface
	 just_f
	 d_then_f_then_g
	 (make_close_enough_p optimism)
	 just_f
	 set_q_pos
	 get_q_pos
	 set_c_pos
	 get_c_pos
	 bound
	 (get_node bound))


let dups sface args =
  (** A* eps optimistic search for domains with duplicate states.
      Requires the search interface [sface] a quality [bound] and a level of
      [optimism] which tells us how aggressive to be in the a* eps phase of the
      search *)
  let bound = Search_args.get_float
    "Aseps_optimistic_search.dups" args 0
  and optimism = Search_args.get_float
    "Aseps_optimistic_search.dups" args 1 in
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd)
    ~key:(wrap sface.Search_interface.key)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_root sface.Search_interface.initial)
    just_f
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Aseps_optimistic.dups
	 search_interface
	 just_f
	 d_then_f_then_g
	 (make_close_enough_p optimism)
	 just_f
	 set_q_pos
	 get_q_pos
	 set_c_pos
	 get_c_pos
	 bound
	 (get_node bound))


let delay_dups sface args =
  (** A* eps optimistic search for domains with duplicate states.
      Requires the search interface [sface] a quality [bound] and a level of
      [optimism] which tells us how aggressive to be in the a* eps phase of the
      search.  Duplicates will not be re-explored until the cleanup phase of
      the search. *)
  failwith "Not Implemented"
  (*
  let bound = Search_args.get_float
    "Aseps_optimistic_search.delaydups" args 0
  and optimism = Search_args.get_float
    "Aseps_optimistic_search.delay_dups" args 1 in
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd)
    ~key:(wrap sface.Search_interface.key)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_root sface.Search_interface.initial) in
    Limit.unwrap_sol6 unwrap_sol
      (Aseps_optimistic.delay_dups
	 search_interface
	 just_f
	 d_then_f_then_g
	 (make_close_enough_p optimism)
	 just_f
	 set_q_pos
	 get_q_pos
	 set_c_pos
	 get_c_pos
	 bound
	 (get_node bound))*)
(* EOF *)


(** Optimistic search, first search is ordered by cost dictated from vector *)


let h_ind = 0
and d_ind = 1
and g_ind = 2
and depth_ind = 3
and h_rev = 4
and d_rev = 5


type 'a node =
    { data : 'a;
      features : float array;
      cost : float;
      f : float;
      mutable p_pos: int;
      mutable f_pos: int;
      mutable d_pos: int;
}


let set_pq_pos n i =
  (** Updates the [p_pos] of node [n], setting it to [i],
      [p_pos] is the nodes position in the optimistic queue. *)
  n.p_pos <- i


let get_pq_pos n =
  (** returns the current [p_pos] of node [n] *)
  n.p_pos


let set_f_pos n i =
  (** Updates the [f_pos] of node [n], setting it to [i].
      [f_pos] is the nodes position in the cleanup queue *)
  n.f_pos <- i

let get_f_pos n =
  (** returns the current [f_pos] of node [n] *)
  n.f_pos

let set_d_pos n i =
  (** Updates the [d_pos] of node [n], setting it to [i].
      [d_pos] is the nodes position in the delay queue *)
  n.d_pos <- i

let get_d_pos n =
  (** returns the current [d_pos] of node [n] *)
  n.d_pos


let better_p a b =
  (** Determines which of the nodes represents a better solution *)
  a.features.(g_ind) <= b.features.(g_ind)


let ordered_p a b =
  (** sorts nodes in order of estimated f, breaking ties in favor of
      high g values *)
  a.cost < b.cost ||
    (a.cost = b.cost &&
       (not (better_p a b)))


let unwrap_sol n =
  (** Decomposes the solution [s] into a form which the domains are expecting
      when doing validation *)
  match n with
    Limit.Nothing -> None
  | Limit.Incumbent (q, n) -> Some (n.data, n.features.(g_ind))


let calc_cost vector features =
  (** taking a weight [vector] and a [features] array, returs an
      estimate of the true cost to go from this node *)
  (assert ((Array.length vector) = (Array.length features)));
  let sum = ref 0. in
    for i = 0 to ((Array.length vector) - 1)
    do
      sum := !sum +. vector.(i) *. features.(i);
    done;
    !sum


let wrap_expand expand hd vector =
  (** Returns an expand function to be used by optimistic framework
      requires the domain [expand] a cost and distance heuristic estimator
      [hd] and the weight [vector] which was learned offline *)
  (fun n ->
     let nd = n.features.(depth_ind) +. 1. in
       List.map (fun (c,g) ->
		   (let (h,d) = hd c in
		    let feat = [| h; g; d; nd; |] in
		      { data = c;
			features = feat;
			cost = calc_cost vector feat;
			f = feat.(g_ind) +. feat.(h_ind);
			p_pos = Dpq.no_position;
			f_pos = Dpq.no_position;
			d_pos = Dpq.no_position;}))
	 (expand n.data n.features.(g_ind)))


let make_init data =
  (** returns the root of the search space *)
  { data = data;
    features = [| infinity; infinity; 0.; 0.|];
    cost = neg_infinity;
    f = neg_infinity;
    p_pos = Dpq.no_position;
    f_pos = Dpq.no_position;
    d_pos = Dpq.no_position;}


let wrap fn =
  (** Wraps a function [f] which works on domain data and makes it so
      that it can be applied to nodes *)
  (fun n -> fn n.data)


let ordered_f a b =
  (** are nodes in f order *)
  a.f < b.f


let get_node fq pq i bound =
  (** determines which node to expand during the cleanup phase.
      pulls some node from either [fq] the cleanup queue, or [pq] the
      optimistic queue. Raises done when incumbent is within the bound.*)
  let fn = Dpq.peek_first fq
  and incumbent = i.Limit.incumbent in
    match incumbent with
	Limit.Nothing ->
	  raise Optimistic_framework.NoIncumbent
      | Limit.Incumbent(qual,inc) ->
	  if (fn.f *. bound) >= inc.f
	  then raise Optimistic_framework.Done;
	  let fpn = Dpq.peek_first pq in
	    if fpn.cost < inc.f then
	      (Dpq.remove pq fpn.p_pos;
	       Dpq.remove fq fpn.f_pos;
	       fpn)
	    else
	      (let trf = fn.f_pos
	       and trp = fn.p_pos in
		 Dpq.remove fq trf;
		 Dpq.remove pq trp;
		 fn)


(************************************ Searches ******************************)

let no_dups sface args =
  (** Performs a search in domains where there are no duplicates.
      [sface] is the domain's search interface
      [vector] is a list of weights to be used *)
  let bound = Search_args.get_float "Vector_optimistic.no_dups" args 0
  and vector = Search_args.get_float_array "Vector_optimistic.no_dups"
    (Array.sub args 1 ((Array.length args) - 1)) in
  let search_interface = Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd vector)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_init sface.Search_interface.initial)
  better_p
  (Limit.make_default_logger (fun n -> n.f)
   (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol5 unwrap_sol
      (Optimistic_framework.no_dups
	 search_interface
	 get_node
	 ordered_p
	 bound
	 better_p
	 ordered_f
	 set_pq_pos
	 set_f_pos)


let dups sface args =
  (** Performs a search in domains where there are no duplicates.
      [sface] is the domain's search interface
      [vector] is a list of weights to be used *)
  let bound = Search_args.get_float "Vector_optimistic.dups" args 0
  and vector = Search_args.get_float_array "Vector_optimistic.dups"
    (Array.sub args 1 ((Array.length args) - 1)) in
  let search_interface = Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd vector)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~key:(wrap sface.Search_interface.key)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_init sface.Search_interface.initial)
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
      Limit.unwrap_sol6 unwrap_sol
	(Optimistic_framework.dups
	   search_interface
	   get_node
	   ordered_p
	   bound
	   better_p
	   ordered_f
	   set_pq_pos
	   set_f_pos
	   get_pq_pos
	   get_f_pos)


let delay_dups sface args =
  (** Performs a search in domains where there are no duplicates.
      Duplicate states are delayed until the cleanup phase, where they
      are then reexpanded.
      [sface] is the domain's search interface
      [vector] is a list of weights to be used *)
  let bound = Search_args.get_float "Vector_optimistic.delay_dups" args 0
  and vector = Search_args.get_float_array "Vector_optimistic.delay_dups"
    (Array.sub args 1 ((Array.length args) - 1)) in
  let search_interface = Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd vector)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_init sface.Search_interface.initial)
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
      Limit.unwrap_sol6 unwrap_sol
	(Optimistic_framework.delay
	   search_interface
	   get_node
	   ordered_p
	   bound
	   better_p
	   ordered_f
	   set_pq_pos
	   set_d_pos
	   set_f_pos
	   get_pq_pos
	   get_d_pos
	   get_f_pos)

(* EOF *)

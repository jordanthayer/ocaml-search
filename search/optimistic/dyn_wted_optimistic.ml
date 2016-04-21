(** Optimistic Search *)


type 'a node = {
  mutable fp : float;
  h : float;
  depth: float;
  mutable g : float;
  mutable ppos : int;
  mutable fpos: int;
  mutable dpos: int;
  data : 'a;
}


let ordered_f a b =
  (** determines if [a] and [b] are in f-order *)
  (a.g +. a.h) <= (b.g +. b.h)


let ordered_p a b =
  (** are [a] and [b] in best-first order*)
  let af = a.g +. a.h
  and bf = b.g +. b.h in
  (a.fp < b.fp) ||
  ((a.fp = b.fp) &&
   ((af < bf) ||
    ((af = bf) &&
     (a.g >= b.g))))


let better_p a b =
  (** is [a] a better incumbent solution than [b] *)
  ordered_f a b


let get_f_pos a =
  (** returns the f position of node [a] *)
  a.fpos

let get_d_pos a =
  (** returns the d position of node [a] *)
  a.dpos

let get_pq_pos a =
  (** returns the position of node [a] in open *)
  a.ppos


let set_f_pos a i =
  (** sets the f position of node [a] to [i] *)
  a.fpos <- i


let set_pq_pos a i =
  (** sets the position of node [a] in open to [i] *)
  a.ppos <- i

let set_d_pos a i =
  (** sets the delayed position of node [a] to [i] *)
  a.dpos <- i


let make_expand expand h weight initial_d =
  (** Converts a domain [expand] function into a search expand function.
      Requires a cost to go estimator [h], an optimisti [weight] and an
      estimate of the distance of the root node from the goal [initial_d]*)
  let make_child =
    (fun dep (n, g) ->
       let hv = h n in
       let fact = Math.fmax 1. (weight *. (1. -. dep /. initial_d)) in
	 { fp = g +.  fact *. hv;
	   h = hv;
	   g = g;
	   depth = dep;
	   fpos = Dpq.no_position;
	   ppos = Dpq.no_position;
	   dpos = Dpq.no_position;
	   data = n;})  in
    (fun parent ->
       List.map (make_child (parent.depth +. 1.))(expand parent.data parent.g))


let wrap f =
  (** Takes a function to be applied to domain nodes and makes it so that
      it may be applied to the search nodes *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Takes the solution from the search space and returns it in domain space*)
  match s with
    Limit.Incumbent (q,n) -> Some (n.data, n.g)
    | _ -> None


let get_node fq pq i bound =
  (** Returns the next node to be expanded *)
  let fn = Dpq.peek_first fq
  and incumbent = i.Limit.incumbent in
    match incumbent with
	Limit.Nothing ->
	  raise Optimistic_framework.NoIncumbent
      | Limit.Incumbent(qual,inc) ->
	  if ((fn.g +. fn.h) *. bound) >= (inc.g +. inc.h)
	  then raise Optimistic_framework.Done;
	  let fpn = Dpq.peek_first pq in
	    if fpn.fp < (inc.g +. inc.h) then
	      (Dpq.remove pq fpn.ppos;
	       Dpq.remove fq fpn.fpos;
	       fpn)
	    else
	      (let trf = fn.fpos
	       and trp = fn.ppos in
		 Dpq.remove fq trf;
		 Dpq.remove pq trp;
		 fn)

(***************************************************************************)

let no_dups sface args =
  (** Performs an optimistic dynamicly weighted A* search on domains with
      few to no duplicate nodes.
      [sface]
      [wt]
      [bound] *)
  let bound = Search_args.get_float "Dyn_wted_optimistic.no_dups" args 0
  and wt = Search_args.get_float "Dyn_wted_optimistic.no_dups" args 1 in
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h wt
		    (sface.Search_interface.d sface.Search_interface.initial))
    ~key:(wrap sface.Search_interface.key)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { fp = neg_infinity;
      h = neg_infinity;
      g = 0.;
      depth = 0.;
      ppos = Dpq.no_position;
      fpos = Dpq.no_position;
      dpos = Dpq.no_position;
      data = sface.Search_interface.initial}
    better_p
    (Limit.make_default_logger (fun n -> n.g +. n.h)
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
  (** Performs an optimistic dynamicly weighted A* search on domains with
      many duplicate nodes.
      [sface]
      [wt]
      [bound] *)
  let bound = Search_args.get_float "Dyn_wted_optimistic.dups" args 0
  and wt = Search_args.get_float "Dyn_wted_optimistic.dups" args 1 in
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h wt
		    (sface.Search_interface.d sface.Search_interface.initial))
    ~key:(wrap sface.Search_interface.key)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { fp = neg_infinity;
      h = neg_infinity;
      g = 0.;
      depth = 0.;
      ppos = Dpq.no_position;
      fpos = Dpq.no_position;
      dpos = Dpq.no_position;
      data = sface.Search_interface.initial}
    better_p
    (Limit.make_default_logger (fun n -> n.g +. n.h)
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
  (** Performs an optimistic dynamicly weighted A* search on domains with
      many duplicate nodes.  Duplicate states are delayed until cleanup
      [sface]
      [wt]
      [bound] *)
  let bound = Search_args.get_float "Dyn_wted_optimistic.delay_dups" args 0
  and wt = Search_args.get_float "Dyn_wted_optimistic.delay_dups" args 1 in
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h wt
		    (sface.Search_interface.d sface.Search_interface.initial))
    ~key:(wrap sface.Search_interface.key)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    { fp = neg_infinity;
      h = neg_infinity;
      g = 0.;
      depth = 0.;
      ppos = Dpq.no_position;
      fpos = Dpq.no_position;
      dpos = Dpq.no_position;
      data = sface.Search_interface.initial}
    better_p
    (Limit.make_default_logger (fun n -> n.g +. n.h)
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

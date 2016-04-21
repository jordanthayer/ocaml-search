(** Optimistic Search *)

open Optimistic_search


let make_expand expand hd wt initial_d =
  (** Takes the [expand] function for the domain nodes and converts it into
      an expand function which will operate on search nodes.  [hd] is a cost
      distance estimator.  [wt] is the optimistic weight, and [initial_d] is
      the estimated distance from the root node to a goal. *)
  let make_child =
    (fun (n, g) ->
       let h,d = hd n in
       let factor = Math.fmax 1.
	 (Math.fmin wt (wt *. (d /. initial_d))) in
	 { fp = g +. factor *. h;
	   h = h;
	   g = g;
	   fpos = Dpq.no_position;
	   ppos = Dpq.no_position;
	   dpos = Dpq.no_position;
	   data = n;})  in
    (fun parent ->
       List.map make_child (expand parent.data parent.g))


(***************************************************************************)

let no_dups sface args =
  (** Performs DwA* optimistic search on domains with few / no duplicates.
      [sface] is the search interface, [bound] is the desired quality bound and
      [wt] is the optimistic factor *)
  let bound = Search_args.get_float "Dwa_optimistic.no_dups" args 0
  and wt = Search_args.get_float "Dwa_optimistic.no_dups" args 1 in
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd wt
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
  (** Performs DwA* optimistic search on domains with duplicates.
      [sface] is the search interface, [bound] is the desired quality bound and
      [wt] is the optimistic factor *)
  let bound = Search_args.get_float "Dwa_optimistic.dups" args 0
  and wt = Search_args.get_float "Dwa_optimistic.dups" args 1 in
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd wt
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
  (** Performs DwA* optimistic search on domains with duplicates.
      [sface] is the search interface, [bound] is the desired quality bound and
      [wt] is the optimistic factor.  Duplicate nodes are not explored until
      the cleanup phase of the search*)
  let bound = Search_args.get_float "Dwa_optimistic.delay_dups" args 0
  and wt = Search_args.get_float "Dwa_optimistic.delay_dups" args 1 in
  let search_interface =  Search_interface.make
    ~node_expand:(make_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd wt
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

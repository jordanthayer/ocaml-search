(** Sorts the focal list based on an estimate of true cost to go, found by
    multiplying a weight vector over a set of features available at the given
    node.  Vector is typically gotten through offline regression.  Search is
    carried out in an A* epsilon like manner.
*)

open Vector_search
open Clamped_vector_search

let make_close_enough wt =
  (** Close enough predicate required by Geq.ml *)
  (fun a b ->
     (wt *. a.f) >= b.f)


let ordered_2 a b =
  (** Ordering predicate based on node cost *)
  a.cost < b.cost

(*************************************************************************)


let no_dups sface args =
  (** Performs a search in domains where there are no duplicates.
      [sface] is the domain's search interface
      [vector] is a list of weights to be used *)
  let bound = Search_args.get_float "Vector_aseps.no_dups" args 0
  and vector = Search_args.get_float_array "Vector_aseps.no_dups"
    (Array.sub args 1 ((Array.length args) - 1)) in
    let search_interface = Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd
		    sface.Search_interface.rev_hd vector bound)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_init sface.Search_interface.initial)
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length))
    in
    Limit.unwrap_sol5 unwrap_sol
      (Focal_search.search
	 search_interface
	 ordered_p
	 (make_close_enough bound)
	 ordered_2
	 better_p
	 setpos
	 getpos)


let dups sface args =
  (** Performs a search in domains where there are no duplicates.
      [sface] is the domain's search interface
      [vector] is a list of weights to be used *)
  let bound = Search_args.get_float "Vector_aseps.dups" args 0
  and vector = Search_args.get_float_array "Vector_aseps.dups"
    (Array.sub args 1 ((Array.length args) - 1)) in
  let search_interface = Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.hd
		    sface.Search_interface.rev_hd vector bound)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~key:(wrap sface.Search_interface.key)
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    sface.Search_interface.domain
    (make_init sface.Search_interface.initial)
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    Limit.unwrap_sol6 unwrap_sol
      (Focal_search.search_dups
	 search_interface
	 ordered_p
	 (make_close_enough bound)
	 ordered_2
	 better_p
	 setpos
	 getpos)


(* EOF *)

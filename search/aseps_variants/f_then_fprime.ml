(** An A* epsilon variant which orders the focal list on wA* criteria
    instead of d.
    Jordan - July 2009
*)

open F_then_dwa

let wrap_expand expand h wt =
  (** takes a domain [expand] function and a cost and distance estimator [hd]
      and returns a node expand function.  [Wt] is the weight to be used on
      the focal cost function, and [rt] is the root of the search space. *)
  (fun n ->
     List.map (fun (n, g) ->
		 let hv = h n in
		   { f = g +. hv;
		     g = g;
		     fp = wt *. hv +. g;
		     q_pos = Dpq.no_position;
		     data = n; })
     (expand n.data n.g))

(****************** Searches *******************)
let no_dups sface args =
  (** A* epsilon search on domains without many duplicates.
      [sface] the domain interface
      [wt] the desired suboptimality bound
      [agg] tells us how aggressive the focal ordereing should be *)
  let wt = Search_args.get_float "F_then_fprime.no_dups" args 0
  and agg = Search_args.get_float "F_then_fprime.no_dups" args 1 in
  let search_interface = Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h agg)
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
    Limit.unwrap_sol5 unwrap
      (Focal_search.search
	 search_interface
	 just_f
	 (make_close_enough_p wt)
	 fp_then_f_then_g
	 just_f
	 set_q_pos
	 get_q_pos)


let dups sface args =
  (** A* epsilon search on domains with many duplicates.
      [sface] the domain interface
      [wt] the desired suboptimality bound
      [agg] tells us how aggressive the focal ordereing should be *)
  let wt = Search_args.get_float "F_then_fprime.dups" args 0
  and agg = Search_args.get_float "F_then_fprime.dups" args 1 in
  let search_interface = Search_interface.make
    ~node_expand:(wrap_expand sface.Search_interface.domain_expand
		    sface.Search_interface.h agg)
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
  Limit.unwrap_sol6 unwrap
    (Focal_search.search_dups
       search_interface
       just_f
       (make_close_enough_p wt)
       fp_then_f_then_g
       just_f
       set_q_pos
       get_q_pos)


(* Drop dups would not be admissible and so is not included *)

(* EOF *)

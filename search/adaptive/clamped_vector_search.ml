(** Clamped vector search extends vector search in that the solutions returned
    from this framework will be within a bounded factor of optimal; it is
    a bounded suboptimal version of the inadmissible vector search.  Still
    requires a vector of weight to be applied to the features of the search
    in order to come up with a suitable cost function *)

open Vector_search

let wrap_expand expand hd rev_hd vector bound =
  (** Builds a node expand funciton, which will take the state out of the node
      run the domain expand function [expand].  [hd] provides a cost to go
      and distance to go estimate.  These must be admissible for the search
      to return bonded resolts. [vector] is the weight vector used to
      calculate the cost of a node.  [bound] is the desired quality bound with
      a value of 1 representing optimal and 2 representing within 200% of the
      cost of optimal *)
  (fun n ->
     let nd = n.features.(depth_ind) +. 1. in
       List.map (fun (c,g) ->
		   (let (h,d) = hd c
		    and (rh,rd) = rev_hd c in
		    let feat = [| h; g; d; nd; rh; rd;|] in
		    let f = feat.(g_ind) +. feat.(h_ind) in
		      { data = c;
			features = feat;
			cost = min (calc_cost vector feat) (bound *. f);
			f = f ;
			pos = Dpq.no_position;}))
	 (expand n.data n.features.(g_ind)))


(******************************** Searches *********************************)

let no_dups sface bound vector =
  (** Performs a clampped vector search on domains with no duplicate
      nodes.*)
  output vector;
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
      (Best_first.search
	 search_interface
	 ordered_p
	 better_p)


let dups sface bound vector =
  (** Performs a clamped vector search on domains with a fair amount of
      duplicate states. *)
    output vector;
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
      (Best_first.search_dups
	 search_interface
	 ordered_p
	 better_p
	 setpos
	 getpos)


let drop_dups sface bound vector =
  (** Performs a clamped vector search on domains with many duplicates,
      ignoring duplicate states whenever they are encountered. *)
    output vector;
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
      (Best_first.search_drop_dups
	 search_interface
	 ordered_p
	 better_p
	 setpos
	 getpos)


(* EOF *)

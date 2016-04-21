(** Weighted extension of the IDA* algorithm *)

open Iterative_deepening_astar

let make_expand w expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (c, g) ->
		 let hv = h c in
		 { data = c;
		   g = g;
		   h = hv;
		   f = g +. w *. hv}) (expand n.data n.g))


let dups sface args =
  (** Performs a wIDA* search from the initial state to a goal, for
      domains with no duplicates. *)
  let w = Search_args.get_float "Wted_idastar.dups" args 0 in
  let search_interface = Search_interface.make
    ~node_expand:(make_expand w
		    sface.Search_interface.domain_expand
		    sface.Search_interface.h)
    ~goal_p:(wrap sface.Search_interface.goal_p)
    ~halt_on:sface.Search_interface.halt_on
    ~hash:sface.Search_interface.hash
    ~equals:sface.Search_interface.equals
    ~key:(wrap sface.Search_interface.key)
    sface.Search_interface.domain
    { data = sface.Search_interface.initial;
      g = 0.;
      h = 0.;
      f = 0.;}
    better_p
    (Limit.make_default_logger (fun n -> n.f)
       (wrap sface.Search_interface.get_sol_length)) in
    reset_bound_default
      (sface.Search_interface.h sface.Search_interface.initial);
    Limit.unwrap_sol6 unwrap_sol
      (Iterative_deepening.no_dups_in_dups_dom
	 search_interface
	 better_p
	 see_expansion_default
	 check_bound_default
	 iteration_complete_default)


(* EOF *)

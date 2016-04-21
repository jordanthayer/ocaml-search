(** Anytime A* using revised dwa* as the backbone search
    Jordan - July 2009 *)

open Anytime_dwastar


let make_expand expand hd wt initial_d =
  (** Takes the domain [expand] function and a [h]euristic calculator.
      Needs the [wt] which will be applied to the heuristic.
      Creates an expand function which returns search nodes. *)
  (fun n ->
     List.map (fun (d, g) ->
		 let hv,dv = hd d  in
		 let factor = Math.fmax 1.
		   (Math.fmin wt (wt *. (dv /. initial_d))) in
		   { data = d;
		     wf = g +. factor *. hv;
		     f = g +. hv;
		     g = g;
		     depth = n.depth + 1;
		     pos = Dpq.no_position; }) (expand n.data n.g))


(***************** Searches ***********************************)
let make_iface sface wt =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:
      (make_expand sface.Search_interface.domain_expand
	 sface.Search_interface.hd wt
	 (sface.Search_interface.d sface.Search_interface.initial))
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~key:(wrap sface.Search_interface.key)
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	wf = neg_infinity;
	f = neg_infinity;
	g = 0.;
	depth = 0;
	pos = Dpq.no_position;}
      just_f
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


let no_dups sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains with no duplicates. *)
  let wt = Search_args.get_float "Anytime_rdwastar.no_dups" args 0 in
  let search_interface = make_iface sface wt in
    Limit.unwrap_sol5 unwrap_sol
      (Continued_search.no_dups
	 search_interface
	 ordered_p)


let dups sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wt = Search_args.get_float "Anytime_rdwastar.dups" args 0 in
  let search_interface = make_iface sface wt in
  Limit.unwrap_sol6 unwrap_sol
    (Continued_search.dups
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       search_interface
       just_f
       ordered_p
       setpos getpos)


let delay_dups sface args =
  (** Performs a weighted A* search from the initial state to a goal,
      for domains where duplicates are frequently encountered. *)
  let wt = Search_args.get_float "Anytime_rdwastar.delay_dups" args 0 in
  let search_interface = make_iface sface wt in
    Limit.unwrap_sol6 unwrap_sol
      (Continued_search.delay_dups
	 (* must have g=0 as base for others, and
	    f<others to prevent re-opening *)
	 search_interface
	 just_f
	 ordered_p
	 setpos getpos)


(* EOF *)

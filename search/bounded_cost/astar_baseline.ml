(**

    @author jordan
    @since 2011-07-06
*)

open Greedy_baseline

let f_then_h a b =
  (** Sorts nodes solely on total cost information *)
  a.f < b.f || ((a.f = b.f) && (a.h <= b.h))

let no_dups sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains with no duplicates. *)
  let cost_bound = Search_args.get_float "Speedy_baseline" args 0 in
  let dummy_incumbent = { data = sface.Search_interface.initial;
			  h = nan;
			  f = cost_bound;
			  g = cost_bound;
			  pos = -1337;} in
  let h = sface.Search_interface.h sface.Search_interface.initial in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~incumbent:(Limit.Incumbent (0., dummy_incumbent))
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	h = h;
	f = h;
	g = 0.;
	pos = Dpq.no_position;}
    just_f
    (Limit.make_default_logger (fun n -> n.g)
       (wrap sface.Search_interface.get_sol_length)) in
	Limit.unwrap_sol5 unwrap_sol
      (Best_first.search
	 search_interface
	 f_then_h
	 just_f)


let dups sface args =
  (** Performs a greedy search from the initial state to a goal,
      for domains with no duplicates. *)
  let cost_bound = Search_args.get_float "Speedy_baseline" args 0 in
  let dummy_incumbent = { data = sface.Search_interface.initial;
			  h = nan;
			  f = cost_bound;
			  g = cost_bound;
			  pos = -1337;} in
  let h = sface.Search_interface.h sface.Search_interface.initial in
  let search_interface =
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~key:(wrap sface.Search_interface.key)
      ~incumbent:(Limit.Incumbent (0., dummy_incumbent))
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	h = h;
	f = h;
	g = 0.;
	pos = Dpq.no_position;}
    just_f
    (Limit.make_default_logger (fun n -> n.g)
       (wrap sface.Search_interface.get_sol_length)) in
	Limit.unwrap_sol6 unwrap_sol
      (Best_first.search_dups
	 search_interface
	 f_then_h
	 just_f
	 setpos
	 getpos)



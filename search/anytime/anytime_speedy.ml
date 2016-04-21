(**

    @author jtd7
    @since 2010-05-20

   Continued speedy search
*)

open Anytime_greedy


let make_iface sface =
  let def_log = Limit.make_default_logger (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.d)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~key:(wrap sface.Search_interface.key)
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      { data = sface.Search_interface.initial;
	h = neg_infinity;
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
  Limit.unwrap_sol5 unwrap_sol
    (Continued_search.no_dups
       (make_iface sface)
       ordered_p)


let dups sface args =
  Limit.unwrap_sol6 unwrap_sol
    (Continued_search.dups
       (* must have g=0 as base for others, and
	  f<others to prevent re-opening *)
       (make_iface sface)
       just_f
       ordered_p
       setpos getpos)


(* EOF *)

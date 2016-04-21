(**
    @author jtd7
    @since 2010-05-20

   Baseline anytime algorithm, two phaser which runs greedy then A*
   back to back.

*)

type 'a node =
    {cost : float;
     g : float;
     data : 'a;
     depth : int;
     mutable pos:  int;}


let wrap f =
  (** takes a function to be applied to the data payload
      such as the goal-test or the domain heuristic and
      wraps it so that it can be applied to the entire
      node *)
  (fun n -> f n.data)


let unwrap_sol s =
  (** Unwraps a solution which is in the form of a search node and presents
      it in the format the domain expects it, which is domain data followed
      by cost *)
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let ordered_p a b =
  (a.cost : float) <= b.cost

let better_p a b =
  a.g < b.g

let setpos n i =
  n.pos <- i

let getpos n =
  n.pos


let make_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let nd = n.depth + 1 in
     List.map (fun (d, g) -> { data = d;
			       cost = g +. (h d);
			       g = g;
			       depth = nd;
			       pos = Dpq.no_position; }) (expand n.data n.g))

let make_sface time sface incumbent =
  let def_log =
    Limit.make_default_logger ~silent:true ~time:time (fun n -> n.g)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      ~incumbent:(Limit.Incumbent (0.,incumbent))
      sface.Search_interface.domain
      {data = sface.Search_interface.initial;
       cost = neg_infinity;
       g = 0.;
       depth = 0;
       pos = Dpq.no_position;}
      better_p
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)

(****************************************************************************)


let two_phase sface args =
  let stime = Sys.time() in
    match Greedy.no_dups sface [||] with
	None,x2,x3,x4,x5 -> None,x2,x3,x4,x5
      | Some (data, cost),x2,x3,x4,x5 ->
	  Limit.unwrap_sol5
	    unwrap_sol
	    (let search_interface = make_sface stime sface
	       {cost = cost;
		g = cost;
		data = data;
		depth = 0;
		pos = Dpq.no_position;} in
	       Limit.incr_gen_n search_interface.Search_interface.info x3;
	       Limit.incr_exp_n search_interface.Search_interface.info x2;
	       Limit.incr_prune_n search_interface.Search_interface.info x4;
	       Limit.curr_q search_interface.Search_interface.info x5;
	       Best_first.search
		 search_interface
		 ordered_p
		 better_p)



let two_phase_dups sface args =
  let stime = Sys.time() in
    match Greedy.dups sface [||] with
	None,x2,x3,x4,x5,x6 -> None,x2,x3,x4,x5,x6
      | Some (data, cost),x2,x3,x4,x5,x6 ->
	  Limit.unwrap_sol6
	    unwrap_sol
	    (let search_interface = make_sface stime sface
	       {cost = cost;
		g = cost;
		data = data;
		depth = 0;
		pos = Dpq.no_position;} in
	       Limit.incr_gen_n search_interface.Search_interface.info x3;
	       Limit.incr_exp_n search_interface.Search_interface.info x2;
	       Limit.incr_prune_n search_interface.Search_interface.info x4;
	       Limit.incr_dups_n search_interface.Search_interface.info x6;
	       Limit.curr_q search_interface.Search_interface.info x5;
	       Best_first.search_dups
		 search_interface
		 ordered_p
		 ordered_p
		 setpos
		 getpos)



(* EOF *)

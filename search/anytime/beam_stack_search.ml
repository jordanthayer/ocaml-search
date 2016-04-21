(** Beam Stack Search *)

type 'a node =
    {f : float;
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
  a.f <= b.f


let better_p a b =
  a.g <= b.g (*|| (a.f = b.f && a.g < b.g)*)


let make_expand expand h =
  (** Takes the domain expand function and a heuristic calculator
      and creates an expand function which returns search nodes. *)
  (fun n ->
     let nd = n.depth + 1 in
     List.map (fun (d, g) -> { data = d;
			       f = g +. (h d);
			       g = g;
			       depth = nd;
			       pos = Dpq.no_position; }) (expand n.data n.g))


let make_sface time sface incumbent =
  let def_log =
    Limit.make_default_logger ~silent:true ~time:time (fun n -> n.f)
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
       f = neg_infinity;
       g = 0.;
       depth = 0;
       pos = Dpq.no_position;}
      better_p
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)


(*

  cmw 5/18/2010

  makes a search interface without a seed (assumes the best known
  solution is infinitely far away and infinitely bad).  Useful if you
  don't know what the bound is and don't really want to try and find a
  solution becuase finding a solution to use in the first place is not
  easy.

*)
let make_empty_sface time sface =
  let def_log =
    Limit.make_default_logger ~silent:false ~time:time (fun n -> n.f)
    (wrap sface.Search_interface.get_sol_length) in
    Search_interface.make
      ~node_expand:(make_expand sface.Search_interface.domain_expand
		      sface.Search_interface.h)
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~key:(wrap sface.Search_interface.key)
      ~hash:sface.Search_interface.hash
      ~equals:sface.Search_interface.equals
      ~halt_on:sface.Search_interface.halt_on
      ~incumbent:(Limit.Nothing)
      sface.Search_interface.domain
      {data = sface.Search_interface.initial;
       f = neg_infinity;
       g = 0.;
       depth = 0;
       pos = Dpq.no_position;}
      better_p
      (fun i ->
	 sface.Search_interface.info.Limit.log
	   (Limit.unwrap_info (fun n -> n.data) i);
	 def_log i)



let no_dups sface args =
  let beam_width = Search_args.get_int "Beam_stack_search.no_dups" args 0 in
  let stime = Sys.time() in
    match Greedy.no_dups sface [||] with
	None,x2,x3,x4,x5 -> None,x2,x3,x4,x5
      | Some (data, cost),x2,x3,x4,x5 ->
	  Limit.unwrap_sol5
	    unwrap_sol
	    (let search_interface = make_sface stime sface
	       {f = cost;
		g = cost;
		data = data;
		depth = 0;
		pos = Dpq.no_position;} in
	       Limit.incr_gen_n search_interface.Search_interface.info x3;
	       Limit.incr_exp_n search_interface.Search_interface.info x2;
	       Limit.incr_prune_n search_interface.Search_interface.info x4;
	       Limit.curr_q search_interface.Search_interface.info x5;
	       Beam_stack.search search_interface beam_width (fun n -> n.f)
		 ordered_p better_p)


let dups sface args =
  let beam_width = Search_args.get_int "Beam_stack_search.dups" args 0 in
  let stime = Sys.time() in
    match Greedy.dups sface [||] with
	None,x2,x3,x4,x5,x6 -> None,x2,x3,x4,x5,x6
      | Some (data, cost),x2,x3,x4,x5,x6 ->
	  Verb.pe Verb.always "Starting beam search\n%!";
	  Limit.unwrap_sol6
	    unwrap_sol
	    (let search_interface = make_sface stime sface
	       {f = cost;
		g = cost;
		data = data;
		depth = 0;
		pos = Dpq.no_position;} in
	       Limit.incr_gen_n search_interface.Search_interface.info x3;
	       Limit.incr_exp_n search_interface.Search_interface.info x2;
	       Limit.incr_prune_n search_interface.Search_interface.info x4;
	       Limit.incr_dups_n search_interface.Search_interface.info x6;
	       Limit.curr_q search_interface.Search_interface.info x5;
	       Beam_stack.search_dups
		 search_interface
		 beam_width
		 (fun n -> n.f)
		 (fun n -> n.depth)
		 ordered_p
		 better_p)

(*

  calls beam stack search without a seed.

  cmw 5/18/2010

*)
let no_seed sface args =
  let beam_width = Search_args.get_int "Beam_stack_search.no_seed" args 0 in
  let stime = Sys.time() in
    Limit.unwrap_sol6
      unwrap_sol
      (let search_interface = make_empty_sface stime sface in
	 Beam_stack.search_dups
	   search_interface
	   beam_width
	   (fun n -> n.f)
	   (fun n -> n.depth)
	   ordered_p
	   better_p)



let exact_seed sface args =
  let beam_width = Search_args.get_int "Beam_stack_search.no_seed" args 0 in
  let node_capacity = Search_args.get_float "Beam_stack_search.dups" args 1 in
  let depth_bound = node_capacity /. (float_of_int beam_width) in
  let stime = Sys.time() in
    Limit.unwrap_sol6
      unwrap_sol
      (let search_interface = make_empty_sface stime sface in
	 Beam_stack.search_dups
	   ~exact_bound:(Some depth_bound)
	   search_interface
	   beam_width
	   (fun n -> n.f)
	   (fun n -> n.depth)
	   ordered_p
	   better_p)



(* EOF *)

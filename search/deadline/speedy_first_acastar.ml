(**

    @author jtd7
    @since 2011-01-22

   Speedy First Approximate Contract A*, Since that is how we roll
*)

open Contract_astar

let sfa_dups sface args =
  let contract = Search_args.get_int "Speedy_first_acastar.dups" args 0
  and res = Search_args.get_int "Contract_astar.approx_dups" args 1
  and expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.h)
  and root = make_initial sface
  and goal = (fun n -> sface.Search_interface.goal_p n.data)
  and key = (fun n -> sface.Search_interface.key n.data)
  and eq = sface.Search_interface.equals
  and hash = sface.Search_interface.hash in

  let incumbent,exp,gen,prune,max_q,dupls = (Greedy_adaptive.speedy_drop_int
					       sface [||]) in
  let incumbent =  (match incumbent with
		      | Limit.Nothing -> Limit.Nothing
		      | Limit.Incumbent (_,i) ->
			  Limit.Incumbent
			    (0., { data = i.Greedy_adaptive.data;
				   f = i.Greedy_adaptive.g;
				   g = i.Greedy_adaptive.g;
				   depth = i.Greedy_adaptive.depth;
				   q_pos = Dpq.no_position;} )) in

  let info =  (Limit.make incumbent sface.Search_interface.halt_on ordered_p
		 (Limit.make_default_logger ~silent:true (fun n -> n.f)
		    (fun n -> sface.Search_interface.get_sol_length n.data))) in

  let contract = contract - info.Limit.expanded in
  let ar = Approx_eq_38.get_kl res sface.Search_interface.domain contract in
    Limit.incr_dups_n info dupls;
    Limit.incr_prune_n info prune;
    Limit.incr_gen_n info gen;
    Limit.incr_exp_n info exp;
    dups ar
      (truncate (Eq_38_tables.depth_est sface.Search_interface.domain))
      (Eq_38_tables.bfactor sface.Search_interface.domain)
      expand root goal info key eq hash;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


let sfa_dups_time sface args =
  let time = Search_args.get_float "Speedy_first_acastar.dups" args 0
  and res = Search_args.get_int "Contract_astar.approx_dups" args 1
  and expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.h)
  and root = make_initial sface
  and goal = (fun n -> sface.Search_interface.goal_p n.data)
  and key = (fun n -> sface.Search_interface.key n.data)
  and eq = sface.Search_interface.equals
  and hash = sface.Search_interface.hash in
  let start = Sys.time () in
  let incumbent,exp,gen,prune,max_q,dupls = (Greedy_adaptive.speedy_drop_int
					       sface [||]) in
  let incumbent =  (match incumbent with
		      | Limit.Nothing -> Limit.Nothing
		      | Limit.Incumbent (_,i) ->
			  Limit.Incumbent
			    (0., { data = i.Greedy_adaptive.data;
				   f = i.Greedy_adaptive.g;
				   g = i.Greedy_adaptive.g;
				   depth = i.Greedy_adaptive.depth;
				   q_pos = Dpq.no_position;} )) in

  let info =  (Limit.make incumbent sface.Search_interface.halt_on ordered_p
		 (Limit.make_default_logger ~silent:true (fun n -> n.f)
		    (fun n -> sface.Search_interface.get_sol_length n.data))) in
  let after_speedy = Sys.time() in
  let remaining_time = time -. (after_speedy -. start) in
  let contract = truncate ((remaining_time /. 60.) *.
			     (Eq_38_tables.nodes_min
				sface.Search_interface.domain)) in
  let ar = Approx_eq_38.get_kl res sface.Search_interface.domain contract in
    Limit.incr_dups_n info dupls;
    Limit.incr_prune_n info prune;
    Limit.incr_gen_n info gen;
    Limit.incr_exp_n info exp;
    dups ar
      (truncate (Eq_38_tables.depth_est sface.Search_interface.domain))
      (Eq_38_tables.bfactor sface.Search_interface.domain)
      expand root goal info key eq hash;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


let sfa_dups_austin sface args =
  let time = Search_args.get_float "Speedy_first_acastar.dups" args 0
  and res = Search_args.get_int "Contract_astar.approx_dups" args 1
  and expand = (make_expand sface.Search_interface.domain_expand
		  sface.Search_interface.h)
  and root = make_initial sface
  and goal = (fun n -> sface.Search_interface.goal_p n.data)
  and key = (fun n -> sface.Search_interface.key n.data)
  and eq = sface.Search_interface.equals
  and hash = sface.Search_interface.hash in
  let start = Sys.time () in
  let old_info = sface.Search_interface.info  in

  let incumbent =  (match old_info.Limit.incumbent with
		      | Limit.Nothing -> Limit.Nothing
		      | Limit.Incumbent (g,node) ->
			  Limit.Incumbent
			    (0., { data = node;
				   f = g;
				   g = g;
				   depth = -1;
				   q_pos = Dpq.no_position;} )) in

  let info =  (Limit.make Limit.Nothing
		 sface.Search_interface.halt_on
		 ordered_p
		 (Limit.make_default_logger
		    ~time:sface.Search_interface.info.Limit.start_time
		    (fun n -> n.f)
		    (fun n -> n.depth))) in
  let after_speedy = Sys.time() in
  let remaining_time = time -. (after_speedy -. start) in
  let contract = truncate ((remaining_time /. 60.) *.
			     (Eq_38_tables.nodes_min
				sface.Search_interface.domain)) in
  let ar = Approx_eq_38.get_kl res sface.Search_interface.domain contract in
    Limit.incr_dups_n info old_info.Limit.duplicates;
    Limit.incr_prune_n info old_info.Limit.pruned;
    Limit.incr_gen_n info old_info.Limit.generated;
    Limit.incr_exp_n info old_info.Limit.expanded;
    (match incumbent with
      | Limit.Nothing -> failwith "No incumbent from speedy search"
      | _ -> Limit.new_incumbent info incumbent);
    dups ar
      (truncate (Eq_38_tables.depth_est sface.Search_interface.domain))
      (Eq_38_tables.bfactor sface.Search_interface.domain)
      expand root goal info key eq hash;
    Limit.unwrap_sol6 unwrap_sol (Limit.results6 info)


(* EOF *)

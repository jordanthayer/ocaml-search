(** A Collection of inadmissible heuristics for the TSP problem *)

let make_greedy_dep p symm =
  (** finds the cost of the greedy assignment from this point on *)
  let find_shortest c_loc remaining =
    let nearest = fst (List.fold_left (fun (city,cost) n ->
					 if p.(c_loc).(n) < cost
					 then (n,p.(c_loc).(n))
					 else (city,cost))
			 (-1,infinity) remaining) in
      nearest, List.filter (fun n -> n <> nearest) remaining in

  let rec calc c_loc rem =
    match rem with
	[] -> 0.
      | _ -> (let next, remaining = find_shortest c_loc rem in
		p.(c_loc).(next) +. (calc next remaining)) in

    (fun n ->
       calc n.Tsp.location n.Tsp.remaining)


let make_greedy p symm =
  let nearest_index = ref (-1)
  and nearest_cost = ref infinity
  and current = ref (-1)
  and cost = ref 0. in
    (fun n ->
       let remaining = Array.of_list n.Tsp.remaining in
       let r_count = (Array.length remaining) - 1 in
	 cost := 0.;
	 current := n.Tsp.location;
	 for count = 0 to r_count do
	   (nearest_cost := infinity;
	    let cost_ar = p.(!current) in
	      for consider = 0 to r_count do
		(let city = remaining.(consider) in
		   if city >= 0
		   then (let cost = cost_ar.(city) in
			   if cost < !nearest_cost
			   then (nearest_cost := cost;
				 nearest_index := consider;
				 current := city)))
	      done;
	      cost := !cost +. !nearest_cost;
	      nearest_cost := infinity;
	      remaining.(!nearest_index) <- -1;)
	 done;
	 !cost)



let mixed_admiss_greedy p symm =
  let admiss_h = Tsp.make_h_mst p symm
  and greedy = make_greedy p symm
  and d = Tsp.make_d p in
    (* note if greedy = admiss, you've solved. *)
  let mixed_h = (fun n -> ((admiss_h n) +. (greedy n)) /. 2.)
  and hd = (fun n -> admiss_h n, d n) in
    admiss_h, d, hd, mixed_h


(* Interfaces *)

let greedy_interface problem lim =
    (Search_interface.make
       ~h:(make_greedy problem.Tsp_instances.dists
	     problem.Tsp_instances.symmetric)
       ~d:(Tsp.make_d problem.Tsp_instances.dists)
       ~t:(fun _ -> 0)
       ~hd:(Tsp.make_hd problem.Tsp_instances.dists
	      problem.Tsp_instances.symmetric)
       ~domain_expand:(Tsp.make_expand problem.Tsp_instances.dists
			 problem.Tsp_instances.symmetric)
       ~key:Tsp.key
       ~key_print:Tsp.key_to_string
       ~goal_p:Tsp.goal_p
       ~halt_on:lim
       ~equals:(fun a b -> a = b)
       ~p_update:Tsp.update_parent
       ~get_sol_length:(fun _ -> Array.length problem.Tsp_instances.dists)
       ~rev_h:(Tsp.make_rev_h problem.Tsp_instances.dists
		 problem.Tsp_instances.symmetric)
       ~rev_d:(Tsp.make_rev_d problem)
       ~rev_hd:(let h = Tsp.make_rev_h problem.Tsp_instances.dists
		  problem.Tsp_instances.symmetric
		and d = Tsp.make_rev_d problem in (fun n -> h n, d n))
       Search_interface.Salesman
       (Tsp.make_initial problem.Tsp_instances.dists
	  problem.Tsp_instances.symmetric)
       (fun _ _ -> false) (fun _ -> ()))


let mixed_interface problem lim =
  let _, d, hd, h = mixed_admiss_greedy problem.Tsp_instances.dists
    problem.Tsp_instances.symmetric in
    (Search_interface.make
       ~h:h
       ~d:d
       ~t:(fun _ -> 0)
       ~hd:hd
       ~domain_expand:(Tsp.make_expand problem.Tsp_instances.dists
			 problem.Tsp_instances.symmetric)
       ~key:Tsp.key
       ~key_print:Tsp.key_to_string
       ~goal_p:Tsp.goal_p
       ~halt_on:lim
       ~equals:(fun a b -> a = b)
       ~p_update:Tsp.update_parent
       ~get_sol_length:(fun _ -> Array.length problem.Tsp_instances.dists)
       ~rev_h:(Tsp.make_rev_h problem.Tsp_instances.dists problem.Tsp_instances.symmetric)
       ~rev_d:(Tsp.make_rev_d problem)
       ~rev_hd:(let h = Tsp.make_rev_h problem.Tsp_instances.dists
		  problem.Tsp_instances.symmetric
		and d = Tsp.make_rev_d problem in (fun n -> h n, d n))
       Search_interface.Salesman
       (Tsp.make_initial problem.Tsp_instances.dists problem.Tsp_instances.symmetric)
       (fun _ _ -> false) (fun _ -> ()))



(* EOF *)

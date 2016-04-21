(**

   Beam search where the probability of selection varies by node
   quality (better nodes have a higher probability of selection) but
   the bad nodes have nonzero chance of getting selected.

*)


type 'a stochastic_beam_node = {
  data : 'a;
  f : float;
  g : float;
  depth: int;
  mutable sample_key : float;
  (* where this node resides in the heap. *)
  mutable heap_index : int;
}


let make_initial initial_state =
  { data = initial_state;
    f = 0.0;
    g = 0.0;
    depth = 0;
    sample_key = 0.0;
    heap_index = Newbeam.closed_index; }

let wrap f =
  (fun n -> f n.data)


let wrap2 f =
  (fun a b -> f a.data b.data)


let key_ordered n1 n2 =
  (**checks to see if the nodes are ordered on their weighted random
  key.*)
  n1.sample_key < n2.sample_key


let f_ordered n1 n2 =
  (**checks to see if 2 nodes are f ordered tie breaking on g*)
  (n1.f < n2.f) ||
    ((n1.f = n2.f) &&
       (n1.g > n2.g))


let h_ordered n1 n2 =
  (**checks to see if 2 nodes are h ordered tie breaking on g.*)
  let h1 = n1.f -. n1.g
  and h2 = n2.f -. n2.g in
    (h1 < h2) ||
      ((h1 = h2) &&
         (n1.g < n2.g))



let record_wrap_expand expand h node_record (depth_bound:int) =
  (fun n ->
     let children = (List.map (fun (d, g) ->
                                 { data = d;
                                   f = h d +. g;
                                   g = g;
				   depth = n.depth + 1;
				   sample_key = 0.0;
                                   heap_index = Newbeam.closed_index; })
                       (expand n.data n.g)) in
       node_record n n children;
       match depth_bound with
	   (-1) -> children
	 | _ ->
	     (List.filter (fun a -> a.depth < depth_bound) children))



let unwrap_sol_node s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)



let stochastic_beam_search
    ?(queue_record = Fn.no_op2)
    ?(prune_tracker = Fn.no_op1)
    ?(prune_printer = Fn.no_op1)
    ?(record = false)
    ?(node_capacity = (-1))
    ?(no_close = false)
    ?(purge_closed = false)

    sif args =
  (*make the initial state*)

  let prune_same_level = ref 0 in
  let prune_deeper_replacement = ref 0 in
  let prune_shallow_replacement = ref 0 in
    (*
      let real_prune_tracker prune_pair =
      (
      prune_tracker prune_pair;
      let newnode, victim = prune_pair in
      if(victim.g = newnode.g) then
      prune_same_level := !prune_same_level + 1
      else if (victim.g < newnode.g) then
      prune_deeper_replacement := !prune_deeper_replacement + 1
      else
      prune_shallow_replacement := !prune_shallow_replacement + 1) in

    *)

  let beam_width = Search_args.get_int "Stochastic beam search beam width"
    args 0 in


  let depth_bound = if(node_capacity = (-1)) then (-1)
  else (node_capacity / beam_width) in

  let closed_list = ref(Htable.create sif.Search_interface.hash
			  sif.Search_interface.equals 10000) in
  let initial = make_initial sif.Search_interface.initial in
  let limit_t = (Limit.make Limit.Nothing sif.Search_interface.halt_on
                   f_ordered
                   (Limit.make_default_logger (fun n -> n.g)
		      (wrap sif.Search_interface.get_sol_length))) in


  let key n = (wrap sif.Search_interface.key) n in
  let ht_add n =
    match no_close with true -> ()
      | false -> Htable.replace !closed_list (key n) n in
  let ht_check n = Htable.mem !closed_list (key n) in


  (*
    let ht_remove n = Htable.remove !closed_list (key n) in
  *)
  let ht_find n = Htable.find !closed_list (key n) in

  let ht_prune_check n =
    if(ht_check n) then
      (
	let incumbent = ht_find n in
	  incumbent.f <= n.f
      )
    else false
  in

  let is_goal n = wrap sif.Search_interface.goal_p n in

  let ht_clear () =
    match purge_closed with
	true -> (closed_list := (Htable.create sif.Search_interface.hash
				   sif.Search_interface.equals 10000))
      | false -> () in

  let exp_recorder = match record with
      true -> let er = Recorders.expansion_recorder
        sif.Search_interface.key_printer
        (fun n -> sif.Search_interface.key n.data)
        (fun n -> n.g)
        (fun n -> 0)
        (fun n -> (n.f -. n.g)) in
        (er limit_t)
    | false -> Fn.no_op3 in
  let (expand_node:'a -> 'a list) = record_wrap_expand sif.Search_interface.domain_expand
    sif.Search_interface.h exp_recorder depth_bound in

  let first_layer = Lq.create key_ordered beam_width initial in

    (*put the initial state into both the open and the closed list*)
    ignore (Lq.insert first_layer initial);
    ht_add initial;

    let backtrack_count = ref 0 in




      let rec expand_layer (nodes_to_expand) =
	(
	  (*make the new hash table*)
	  ht_clear();
	  let next_open = (Htable.create sif.Search_interface.hash
			     sif.Search_interface.equals
			     (2*beam_width)) in
	  let new_layer = Lq.create key_ordered beam_width initial in
	  let new_best_f = ref max_float in
	  let new_worst_f = ref min_float in
	    (*put the new nodes into the hash table.*)
	    Lq.unsafe_iter nodes_to_expand
	      (
		fun parent_node ->
		  Limit.incr_exp limit_t;
		  ht_add parent_node;
		  let children = (expand_node parent_node) in
		    (*in tiles there is no reason there should be no children*)
		    List.iter
		      (
			fun child ->
			  Limit.incr_gen limit_t;
			  if(is_goal child) then
			    (Limit.new_incumbent limit_t (Limit.Incumbent (0., child)))
			  else if (limit_t.Limit.incumbent != Limit.Nothing) then ()
			    (*already hit some kind of computation limit*)
			  else if (Limit.halt_p limit_t) then ()
			  else if(ht_prune_check child) then
			    (
			      Limit.incr_dups limit_t;
			    )
			  else if(Htable.mem next_open (key child)) then
			    (
			      Limit.incr_dups limit_t;
			      let incumbent = Htable.find next_open (key child)
			      in
				if(incumbent.f > child.f)
				then
				  (
				    Htable.replace next_open (key child) child;
				    if(!new_best_f > child.f) then
				      new_best_f := child.f;
				    if(!new_worst_f < child.f) then
				      new_worst_f := child.f

				  )
			    )
			  else
			    (
			      Htable.replace next_open (key child) child;
			      if(!new_best_f > child.f) then
				new_best_f := child.f;
			      if(!new_worst_f < child.f) then
				new_worst_f := child.f

			    )
		      )
		      children;
	      );
	    (*once the possible nodes have been identified, iterate
	      through the nodes and select or reject each node.*)
	    Htable.iter (
	      fun key node ->
		(**this function isn't correct yet, but getting the
		   search working in breadth-first mode shouldn't
		   require this to work.*)
		let u_n = Random.float 1.0 in
		let node_quality = ref
		  (1.0 -. ((node.f -. !new_best_f)
			   /. (!new_worst_f -. !new_best_f))) in
		  (*
		    node_quality := 2.0 ** !node_quality;
		  *)
		  (*
		    if(!node_quality > 990.) then node_quality := 990.0;
		  *)
		  if(!node_quality > 0.999) then node_quality := 0.999;
		  if(!node_quality < 0.001) then node_quality := 0.001;

		  node_quality :=  ((-1.0) /. (log !node_quality));
		  (*
		    Printf.fprintf stderr
		    "node f: %f node quality: %f log quality %f u_n: %f node value: %.20f\n"
		    node.f
		    !node_quality
		    ((-1.0) /. (log !node_quality))
		    u_n
		    (u_n ** (1.0/.(!node_quality)));
		  *)
		  node.sample_key <- (u_n ** (1.0/.(!node_quality)));
		  ignore (Lq.insert new_layer node);
	    ) next_open;

	    if(Lq.empty_p new_layer) then
	      ()
	    else
	      (
		(*
		  Printf.fprintf stderr "all nodes:\n";
		  Htable.iter
		  (
		  fun key node ->
		  Printf.fprintf stderr "%f(%f) " node.f
		  node.sample_key
		  )
		  next_open;
		  Printf.fprintf stderr "\nselected nodes:\n";
		  Lq.unsafe_iter new_layer
		  (
		  fun node ->
		  Printf.fprintf stderr "%f(%f) " node.f
		  node.sample_key;
		  );
		  Printf.fprintf stderr "\n";
		*)
		expand_layer new_layer;
	      )

	) in


	expand_layer (first_layer);
	while (
	  (limit_t.Limit.incumbent = Limit.Nothing) &&
	    (not (Limit.halt_p limit_t))
	)
	do
	  (closed_list := (Htable.create sif.Search_interface.hash
			     sif.Search_interface.equals 10000));
	  backtrack_count:= !backtrack_count+1;
	  expand_layer (first_layer);
	done;


	Datafile.write_pairs stdout
	  ["backtracks",(string_of_int !backtrack_count)];
	Datafile.write_pairs stdout
	  ["same level",(string_of_int !prune_same_level)];
	Datafile.write_pairs stdout
	  ["deeper replacement",(string_of_int !prune_deeper_replacement)];
	Datafile.write_pairs stdout
	  ["shallower replacement",(string_of_int !prune_shallow_replacement)];
	Limit.unwrap_sol6 unwrap_sol_node (Limit.results6 limit_t)
	  


let call_stochastic_beam_search sif args = 
  stochastic_beam_search sif args


let call_stochastic_beam_search_no_closed sif args = 
  stochastic_beam_search ~no_close:true sif args


let call_bounded_stochastic_beam_search sif args =
  let node_capacity = Search_args.get_int "Stochastic beam search beam width"
    args 1 in
    stochastic_beam_search ~node_capacity:node_capacity sif args

let call_bounded_stochastic_beam_search_local_closed sif args = 
  let node_capacity = Search_args.get_int "Stochastic beam search beam width"
    args 1 in
    stochastic_beam_search ~node_capacity:node_capacity
      ~purge_closed:true sif args


let call_bounded_stochastic_beam_search_no_closed sif args =
  let node_capacity = Search_args.get_int "Stochastic beam search beam width"
    args 1 in
  let beam_width = Search_args.get_int "Stochastic beam search beam width"
    args 0 in
  let depth_bound = node_capacity / beam_width in
  let (sol, x2,x3,x4,x5,x6) = 
    Newbeam.gen2_beam_search
      ~silent:true
      ~no_close:true
      ~depth_bound:depth_bound
      sif
      [|(string_of_int beam_width);"f"|] in
  let (sb_sol, sb_x2,sb_x3,sb_x4,sb_x5,sb_x6) = 
    stochastic_beam_search ~node_capacity:node_capacity 
      ~no_close:true
      sif args
  in
    (sb_sol, 
     x2 + sb_x2,
     x3 + sb_x3,
     x4 + sb_x4,
     x5 + sb_x5,
     x6 + sb_x6)

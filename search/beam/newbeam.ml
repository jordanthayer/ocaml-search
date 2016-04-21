(*

  Beam search generation 2. 

*)

type restart_predicate = Restart_none | 
    Restart_h | Restart_f | Restart_indecision_max | 
	Restart_indecision_min | Restart_indecision_all | 
	    Restart_indecision_d | Restart_dynamic | 
		Restart_f_bd | Restart_f_bdd | Restart_f_bd_one


type truth_definition = H_order | F_order


let no_op1 _ = ()

let no_op2 _ _ = ()

let no_op3 _ _ _ = ()


type 'a beam_node = {
  data : 'a;
  f : float;
  g : float;
  depth: int;
  (* where this node resides in the heap. *)
  mutable heap_index : int;
}

let closed_index = (-1)


let make_initial initial_state =
  { data = initial_state;
    f = 0.0;
    g = 0.0;
    depth = 0;
    heap_index = closed_index; }



let sub_zero arr = 
  assert (Array.length arr > 0);
  arr.(0)

let euclidean arr = 
  assert(Array.length arr > 0);
  sqrt(Array.fold_left (fun a b -> a +. b**2.0) 1.0 arr)

let unwrap s = 
  s.data

let unwrap_sol_node s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let wrap f =
  (fun n -> f n.data)


let wrap2 f =
  (fun a b -> f a.data b.data)



let record_wrap_expand 
    expand 
    h 
    node_record 
    (depth_bound:int) 
    (weight:float) 
    (depth_bound_pruning:bool)
    (*in a unit cost domain, the heuristic can be used to do depth
      based pruning.  This flag enables that.*)
    =
  (fun n ->
     let children = (List.map (fun (d, g) ->
                                   { data = d;
                                     f = ((h d) *. weight) +. g;
                                     g = g;
				     depth = n.depth + 1;
                                     heap_index = closed_index; })
                       (expand n.data n.g)) in
       node_record n n children;
       match depth_bound with
	   (-1) -> children
	 | _ ->
	     (List.filter 
		(fun a -> 
		   match depth_bound_pruning with
		       false -> a.depth < depth_bound
		     | true -> (a.f < (float_of_int depth_bound))
			       ) children))



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

let d_ordered n1 n2 =
  (**checks to see if 2 nodes are h ordered tie breaking on g.*)
  let h1 = n1.f -. n1.g
  and h2 = n2.f -. n2.g in
    (h1 < h2) ||
      ((h1 = h2) &&
         (n1.g < n2.g))


let make_expand  
    (ht_add:'a -> unit)
    (ht_check: 'a -> bool)
    (ht_find: 'a -> 'a)
    (ht_remove: 'a -> unit)
    (getpos: 'a -> int)
    open_add 
    open_replace_at
    is_goal expand
    lim_o 
    better_p 
    process_pruned 
    process_unpruned 
    continue_search 
    all_node_process
    =
  (**creates an expand function.

     ht_add - adds something to the hash table
     ht_check - checks if something is in the hash table
     ht_find - gets something out of the hash table
     ht_remove - takes something out of the hash table
     open_add - adds something to the open list
     open_replace_at - replaces something at the specified location
     is_goal - checks to see if something is the goal
     expand - expands a node into a list of children
     lim_o - limit t
     better_p - tells if the first thing is better than the second one
     process_pruned - last rites for a pruned node.

  *)
  let process_pruned_node better_node pruned_node_list =

    List.iter (fun a ->
		 process_pruned (better_node,(a));
		 if ((getpos a) != closed_index) then
		   ht_remove a)
      pruned_node_list in

    fun (n:'a) ->
      (*take care to only remove this node if it is in the child
        queue.*)
      let process_child c =
	all_node_process c;
        (*hit goal*)
        if(is_goal c) then (Limit.new_incumbent lim_o (Limit.Incumbent (0., c)))
          (*already found a solution*)
        else if (lim_o.Limit.incumbent != Limit.Nothing) then ()
          (*already hit some kind of computation limit*)
        else if (Limit.halt_p lim_o) then ()
(*
	else if (not (continue_search ())) then ()
*)
          (*child is a duplicate*)
        else if(ht_check c) then (
          Limit.incr_gen lim_o;
          Limit.incr_dups lim_o;
          let old_node = ht_find c in
            if(better_p c old_node) then (
              (*remove the old node from the HT and replace it with
                the new one.*)
              ht_remove old_node;
              ht_add c;
              let old_node_index = getpos old_node in
                (*if the old node was in one of the heaps, replace it
                  with the new one.  If it wasn't in one of the
                  heaps, add it to the open list.*)
                if(old_node_index != (-1)) then (
                  let removed_nodes = open_replace_at old_node_index c in
                    process_pruned_node c removed_nodes)
		else 
		  (
		    let removed_nodes = open_add c in
		      process_pruned_node c removed_nodes)))
          (*node is a new node.*)
        else
	  (
            Limit.incr_gen lim_o;
            ht_add c;
            let removed_nodes = open_add c in
              process_pruned_node c removed_nodes;
	  )
      in
        (*since this node was expanded it was not pruned, therefore
          should be removed from the pruned node list*)
        List.iter process_child (expand n);

        process_unpruned n;
        Limit.incr_exp lim_o
	      

let replace_at heap1 heap2 equals index n =
  assert (index > 0);
  (*check first heap*)
  if (((Picky_queue.count heap1) >= index) &&
        (equals (Picky_queue.access_at heap1 index) n)) then
    Picky_queue.replace_at heap1 n index
      (*check second heap*)
  else if ((Picky_queue.count heap2) >= index &&
             (equals (Picky_queue.access_at heap2 index) n)) then
    Picky_queue.replace_at heap2 n index
  else 
    failwith (Printf.sprintf "error invalid replacement at index %d" index)


(* 

   beam searches now want:

   beam width in argument 0
   ordering predicate in argument 1

*)
let gen2_beam_search
    ?(queue_record = no_op2)
    ?(prune_tracker = no_op1)
    ?(prune_printer = no_op1)
    ?(record = false)
    ?(restart = Restart_none)
    ?(weight = 1.0)
    ?(purge_close = false)
    ?(no_close = false)
    ?(depth_bound = (-1))
    ?(node_capacity = (-1))
    ?(analysis_mode = false)
    ?(analysis_type = None)
    ?(depth_bound_pruning = false)
    ?(print_best_f = false)
    ?(print_best_h = false)
    ?(silent = false)
    sif
    args =
  (**does a beam search.  Requires the search interface and a list of
     arguments.*)
  let prune_same_level = ref 0 in
  let prune_deeper_replacement = ref 0 in
  let prune_shallow_replacement = ref 0 in
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

  let (mcf:unit -> bool) = 
    (fun () -> 
       let res = ((Memtest.memcheck ()) ()) in
	 match res
	 with 
	     true -> true
	   | false ->       Datafile.write_pairs stdout 
	       ["Out of Memory","true"];
	       false
    ) in

  let beam_width = Search_args.get_int "Newbeam.gen2_beam_search"
    args 0 in

  let closed_list = ref(Htable.create sif.Search_interface.hash
			  sif.Search_interface.equals 10000) in
  let initial = make_initial sif.Search_interface.initial in
  let limit_t = (Limit.make Limit.Nothing sif.Search_interface.halt_on
                   f_ordered
                   (Limit.make_default_logger 
		      ~silent:silent
		      (fun n -> n.g)
		      (wrap sif.Search_interface.get_sol_length))) in
  let better_p = match
    Search_args.get_string "Newbeam.beam_search" args 1  with
        "h" -> h_ordered
      | "f" -> f_ordered
      | "d" -> h_ordered
      | _ -> failwith "error - unknown node sorting predicate" in

  let create_beam () = Picky_queue.create
    ~update_function:(fun n index -> n.heap_index <- index)
    better_p
    (fun n -> n.f)
    beam_width initial in

  let open_list = create_beam () in
  let key n = (wrap sif.Search_interface.key) n in
  let ht_add n = 
    match no_close with true -> () 
      | false -> Htable.replace !closed_list (key n) n in
  let ht_check n = Htable.mem !closed_list (key n) in
  let ht_remove n = Htable.remove !closed_list (key n) in
  let ht_find n = Htable.find !closed_list (key n) in
  let is_goal n = wrap sif.Search_interface.goal_p n in
  let ht_clear () = 
    match purge_close with 
	true -> (closed_list := (Htable.create sif.Search_interface.hash
				   sif.Search_interface.equals 10000))
      | false -> () in

  (*Ideally this hash table should only be created when a restarting
    beam search is used, but I'm not sure how to give it the required
    exposure for a restarting beam search.*)
  let pruned_ht = match restart with
      Restart_indecision_all -> 
	Ht_heap.create 
	  initial
	  sif.Search_interface.hash
	  sif.Search_interface.equals
	  10000
	  euclidean 
	  key
    | Restart_f_bd | Restart_f_bdd | Restart_f_bd_one->
	(assert(depth_bound > 0);
	 assert(node_capacity > 0);

	 let expansion_budget = depth_bound * beam_width in

	   assert (expansion_budget < node_capacity);

	   let reserve_size = (node_capacity - expansion_budget) in
	     Ht_heap.create 
	       ~resize:false
	       initial
	       sif.Search_interface.hash
	       sif.Search_interface.equals
	       reserve_size
	       sub_zero 
	       key)
    | _ -> 
	Ht_heap.create 
	  initial
	  sif.Search_interface.hash
	  sif.Search_interface.equals
	  10000
	  sub_zero 
	  key in

  (*
    three process relating to pruned nodes.  
    Generation - when node is created do initial process on it.
    post-generation - when all nodes have been generated.
    un-generation - if node was expanded it wasn't pruned.
  *)
  let unprune_process, prune_process, post_prune_process = 
    let d_array = Garray.init (fun n -> (-1.0)) in
    let quality = match restart with
	Restart_none -> (fun _ _ -> [|0.0|])
      | Restart_h -> (fun _ n -> [|n.f -. n.g|])
      | Restart_f -> (fun _ n -> [|n.f|])

      | Restart_f_bd | Restart_f_bdd | Restart_f_bd_one->
	  (
	    match args.(2) with
		"f" -> (fun _ n -> [|n.f|])
	      | "h" -> (fun _ n -> [|n.f -. n.g|])
	      | _ -> failwith "invalid resort selection"
	  )
      | Restart_dynamic -> 
	  (fun heap n -> 
	     [|n.f -. (Picky_queue.worst_item_quality heap)|])
      | Restart_indecision_max -> 
	  (fun heap n -> [|n.f -. (Picky_queue.best_item_quality heap)|])
      | Restart_indecision_min -> 
	  (fun heap n -> 
	     [|n.f -. (Picky_queue.worst_item_quality heap)|])
      | Restart_indecision_all -> 
	  (fun heap n -> 
	     [|n.f -. (Picky_queue.worst_item_quality heap);
	       n.f -. n.g;
	       n.f|])
      | Restart_indecision_d -> (fun _ n -> 
				   let this_d = int_of_float 
				     ((wrap sif.Search_interface.d) n) in 
				     [|n.f -. (Garray.get d_array this_d)|])
    in

    let temp_pruned_list = ref [] in
    let temp_unprune_list = ref [] in
    let upp n = match restart with
	Restart_none -> no_op1 n
      | _ -> temp_unprune_list := n :: !temp_unprune_list in 
    let pp (newnode,victim) = match restart with 
	Restart_none -> real_prune_tracker (newnode,victim)
      | Restart_indecision_d -> real_prune_tracker (newnode,victim);
	  temp_pruned_list := victim :: !temp_pruned_list;
	  let this_d = int_of_float ((wrap sif.Search_interface.d) victim) in 
	  let inc_f = Garray.get d_array this_d in
	    if(inc_f = (-1.0)) then Garray.set d_array this_d victim.f
	    else if(inc_f > victim.f) then Garray.set d_array this_d victim.f
	    else ()
      | _ -> real_prune_tracker (newnode, victim);
	  temp_pruned_list := victim :: !temp_pruned_list in
    let ppp heap = 
      List.iter (fun n -> 
		   ignore(Ht_heap.push pruned_ht n (quality heap n)))
	!temp_pruned_list;
      List.iter (fun n -> 
		   Ht_heap.remove_item pruned_ht n;)
	!temp_unprune_list;
      temp_pruned_list := [];
      temp_unprune_list := [];
    in 
      upp,pp,ppp in
  let exp_recorder = match record with
      true -> let er = Recorders.expansion_recorder
        sif.Search_interface.key_printer
        (fun n -> sif.Search_interface.key n.data)
        (fun n -> n.g)
        (fun n -> 0)
        (fun n -> (n.f -. n.g)) in
        (er limit_t)
    | false -> no_op3 in

  let h_fun = match 
    Search_args.get_string "Newbeam.beam_search" args 1 with
	"d" -> sif.Search_interface.d
      | _ -> sif.Search_interface.h in

  let (expand_node:'a -> 'a list) = record_wrap_expand 
    sif.Search_interface.domain_expand
    h_fun
    exp_recorder 
    depth_bound 
    weight 
    depth_bound_pruning
  in

  let layer_ht = ref(Htable.create sif.Search_interface.hash
		       sif.Search_interface.equals 10000) in
  let declare_child c = 
    (*says the child exists, *)
    (
      if(analysis_mode) then 
	(
	  if(Htable.mem !layer_ht (key c))
	  then
	    (*duplicate child, new copy better child*)
	    (*duplicate child, this one is worse, do nothing*)
	    (
	      let old_child = Htable.find !layer_ht (key c) in
		if(old_child.g > c.g) then
		  Htable.replace !layer_ht (key c) c		  
	    )
	      (*new child*)
	  else
	    (
	      Htable.replace !layer_ht (key c) c
	    )
	)
    ) in

  let clear_layer_ht () = 
    if(analysis_mode) then 
      (
	layer_ht := (Htable.create sif.Search_interface.hash
		       sif.Search_interface.equals
		       10000)) in
  let process_layer_ht () = 
    if(analysis_mode) then 
      (
	let order_list = ref [] in
	let best_f = ref max_float in
	let best_h = ref max_float in
	  Htable.iter 
	    (fun _ n -> 
	       let add_to_this_node = match analysis_type with
		   Some H_order -> 0.0
		 | Some F_order -> n.g
		 | None -> failwith "no order selected" in 
		 sif.Search_interface.parent_update (unwrap n) (unwrap n);
		 let nsi = Search_interface.alter ~initial:(Some (unwrap n)) sif in
		 let (sol,_,_,_,_,_) = Astar.dups_silent nsi [||] in
		   (match sol with
			None -> failwith "A* failed to find a solution\n"
		      | Some (_,cost) ->
			  if(!best_f > (cost +. n.g)) then
			    best_f := (n.g +. cost);
			  if(!best_h > (cost)) then
			    best_h := (cost);

			  order_list := (Order.make_n (Some (n))
					   (cost +. add_to_this_node)
					   (n.f -. n.g +. add_to_this_node)) :: !order_list;
		   )
	    ) !layer_ht;
	  if(print_best_f) then 
	    (
	      Datafile.write_pairs stdout 
		["best f",(string_of_float !best_f)];
	    )
	  else if (print_best_h) then
	    (
	      Datafile.write_pairs stdout 
		["best h",(string_of_float !best_h)];
	    )
	  else 
	    (
	      Datafile.write_pairs stdout 
		["disorder %",(string_of_float (Order.check_order_p !order_list))]))
  in

  let rec expand_layer parent_nq downward_progress =
    (*if there is an incumbent stop*)
    if(limit_t.Limit.incumbent != Limit.Nothing) then (downward_progress)
      (*if the queue is empty then stop*)
    else if(Picky_queue.empty_p parent_nq) then (downward_progress)
      (*if some computation limit has been reached then stop*)
    else if(Limit.halt_p limit_t) then (downward_progress)
    else if(not (mcf ())) then (downward_progress)
      (*process this queue*)
    else
      (
	queue_record limit_t parent_nq;
	prune_printer limit_t;
	process_layer_ht ();
	clear_layer_ht ();
	let next_layer = create_beam () in
	let expand_this_layer = make_expand 
	  ht_add ht_check ht_find ht_remove
	  (fun n -> n.heap_index)
          (*open_add*)
          (Picky_queue.insert next_layer)
          (replace_at parent_nq next_layer
	     (wrap2 (fun n1 n2 -> sif.Search_interface.equals
		       (sif.Search_interface.key n1 )
		       (sif.Search_interface.key n2 ))))
          (*open_replace_at*)
          is_goal expand_node limit_t better_p
          prune_process
          unprune_process 
	  mcf
	  declare_child
	in
	  while(not(Picky_queue.empty_p parent_nq) && 
		  not(Limit.halt_p limit_t) &&
		  limit_t.Limit.incumbent = Limit.Nothing)
	  do
	    expand_this_layer (Picky_queue.extract_first parent_nq);
	  done;
	  post_prune_process next_layer;
	  ht_clear ();
	  expand_layer next_layer (downward_progress +. 1.);
      ) in
    (*goal check the initial state, if it is a goal register it as
      incumbent then the other functions will just return.*)
    ignore(Picky_queue.insert open_list initial);
    ht_add initial;
    if(is_goal initial) then (Limit.new_incumbent limit_t
				(Limit.Incumbent (0., initial)));
    (*don't need the return value from the initial beam.*)
    ignore (expand_layer open_list 1.);

    let backtrack_count = ref 0 in
    let rc = Restart_chooser.create 3 in
      while(restart != Restart_none &&
	  limit_t.Limit.incumbent = Limit.Nothing && 
	  not(Limit.halt_p limit_t) &&
	  (Ht_heap.has_more pruned_ht))
      do
	(let action_to_choose = ref 0 in
	   backtrack_count := !backtrack_count + 1;
	   let new_beam = ref (create_beam ()) in 
	     if(restart = Restart_dynamic) then
	       (
		 action_to_choose := Restart_chooser.choose_action rc;
		 match (!action_to_choose) with
		     0 -> (
		       for i=1 to beam_width do
			 if(Ht_heap.has_more pruned_ht) then
			   ignore(Picky_queue.insert !new_beam (Ht_heap.pop
								  pruned_ht));
		       done)
		   | 1 -> (
		       new_beam := Picky_queue.create 
			 ~update_function:(fun n index -> n.heap_index <- index)
			 h_ordered
			 (fun n -> n.f)
			 beam_width initial;
		       Ht_heap.cheap_iter pruned_ht
			 (Picky_queue.insert !new_beam);
		     )
		   | 2 -> (
		       new_beam := Picky_queue.create 
			 ~update_function:(fun n index -> n.heap_index <- index)
			 f_ordered
			 (fun n -> n.f)
			 beam_width initial;
		       Ht_heap.cheap_iter pruned_ht (Picky_queue.insert !new_beam);
		     )
		   | _ -> failwith "invalid action choice";)
	     else if (restart = Restart_f_bdd) then
	       (
		 let best_node = Ht_heap.pop pruned_ht in
		   (*find all the parents that generated the best node*)
		 let parents_count = ref 0 in
		 let to_remove = ref [] in
		 let parents = ref [] in
		   Htable.iter (fun _ a -> 
				  if(a.depth = (best_node.depth - 1))
				  then
				    (
				      parents := a :: !parents;
				      parents_count := !parents_count + 1;
				    )
				  else if (a.depth >= best_node.depth)
				  then
				    (
				      to_remove := a :: !to_remove;
				    );
			       )
		     !closed_list;

		   (*regenerate the children*)
		   let children = ref [] in
		     List.iter (fun a -> 
				  children := 
				    (List.rev_append (expand_node a)
				       !children))
		       !parents; 

		     (*remove the children that were already processed*)
		     children := List.filter (
		       fun a -> not (ht_check a))
		       !children;
		     (*use the list of children to prepare the next
		       beam by comparing it to what is in the closed
		       list and then adding it to the next beam if it
		       is not already in the closed list (add
		       conditionallyif it is already in the closed
		       list and remove the one that is already there
		       in this case.*)
		     List.iter (
		       fun child -> 
			 (*child is a duplicate*)
			 if(ht_check child) then
			   (
			     (*check if this one is better for the
			       non-unit cost case*)
			     let incumbent = ht_find child in
			       if(f_ordered child incumbent) then
				 (
				   (*replace inthe hash table*)
				   ht_remove incumbent;
				   ht_add child;
				   (*remove from the heap and replace
				     with child*)
				   let index = incumbent.heap_index in
				     assert (index >= 0);
				     ignore (Picky_queue.replace_at !new_beam
					       child index);
				 )
			   )
			     (*child is new*)
			 else
			   (
			     ignore (Picky_queue.insert !new_beam
				       child);
			   )
		     ) !children;
		     List.iter (ht_remove) !to_remove;
	       )
	     else if (restart = Restart_f_bd_one) then
	       (
		 (*clear the closed list*)
		 closed_list := 
		   (Htable.create sif.Search_interface.hash
		      sif.Search_interface.equals 10000);
		 (*put ONE node back in the beam*)
		 if(Ht_heap.has_more pruned_ht) then
		   ignore(Picky_queue.insert !new_beam (Ht_heap.pop
							  pruned_ht));
	       )
	     else(
	       match restart with
		   Restart_f_bd -> closed_list := 
		     (Htable.create sif.Search_interface.hash
			sif.Search_interface.equals 10000)
		 | _ -> ();
		     for i=1 to beam_width do
		       if(Ht_heap.has_more pruned_ht) then
			 ignore(Picky_queue.insert !new_beam (Ht_heap.pop
								pruned_ht));
		     done;);
	     Restart_chooser.register_result rc 
	       !action_to_choose (expand_layer !new_beam 1.))
      done;

      if(not silent) then
	(
	  Datafile.write_pairs stdout 
	    ["backtracks",(string_of_int !backtrack_count)];
	  Datafile.write_pairs stdout 
	    ["same level",(string_of_int !prune_same_level)];
	  Datafile.write_pairs stdout 
	    ["deeper replacement",(string_of_int !prune_deeper_replacement)];
	  Datafile.write_pairs stdout 
	    ["shallower replacement",(string_of_int !prune_shallow_replacement)];
	);

      Limit.unwrap_sol6 unwrap_sol_node (Limit.results6 limit_t)



let basic_beam sif args =
  gen2_beam_search sif args

let wted_beam sif args =
  assert ((Array.length args) = 3);
  let wt = Search_args.get_float "new_beam.wted_beam" args 2 in
    gen2_beam_search sif args ~weight:wt


let restarting_beam sif args = 
  assert ((Array.length args) = 3);
  let restart = match args.(2) with
      "f" -> Restart_f
    | "h" -> Restart_h
    | "i_max" -> Restart_indecision_max
    | "i_min" -> Restart_indecision_min
    | "i_all" -> Restart_indecision_all
    | "i_d" -> Restart_indecision_d
    | "i_dyn" -> Restart_dynamic
    | _ -> failwith "invalid restart predicate" in
    gen2_beam_search ~restart:restart sif args

let node_record_beam sif args =
  gen2_beam_search ~record:true sif args


let purge_beam sif args =
  gen2_beam_search ~purge_close:true sif args


let no_close_beam sif args =
  gen2_beam_search 
    ~no_close:true 
    sif args

let ib_beam_bd sif args = 
  let depth_bound = Search_args.get_int "newbeam ib beam" args 3 in
  let node_capacity = Search_args.get_int "newbeam ib beam" args 4 in
    gen2_beam_search
      ~restart:Restart_f_bd
      ~depth_bound:depth_bound
      ~node_capacity:node_capacity
      sif
      args

let ib_beam_bdd sif args = 
  let depth_bound = Search_args.get_int "newbeam ib beam" args 3 in
  let node_capacity = Search_args.get_int "newbeam ib beam" args 4 in
    gen2_beam_search
      ~restart:Restart_f_bdd
      ~depth_bound:depth_bound
      ~node_capacity:node_capacity
      sif
      args

let ib_beam_bd_one sif args = 
  let depth_bound = Search_args.get_int "newbeam ib beam" args 3 in
  let node_capacity = Search_args.get_int "newbeam ib beam" args 4 in
    gen2_beam_search
      ~restart:Restart_f_bd_one
      ~depth_bound:depth_bound
      ~node_capacity:node_capacity
      sif
      args


let ib_beam_bd_one_prune sif args = 
  let depth_bound = Search_args.get_int "newbeam ib beam" args 3 in
  let node_capacity = Search_args.get_int "newbeam ib beam" args 4 in
    gen2_beam_search
      ~restart:Restart_f_bd_one
      ~depth_bound:depth_bound
      ~node_capacity:node_capacity
      ~depth_bound_pruning:true
      sif
      args


let queue_record_beam sif args =
  let queue_recorder = Recorders.pq_recorder sif.Search_interface.key_printer
    (fun n -> sif.Search_interface.key n.data) in
    gen2_beam_search ~queue_record:queue_recorder sif args



let queue_check_beam_f_star sif args = 
    gen2_beam_search 
      ~analysis_mode:true
      ~analysis_type:(Some F_order)
      sif args

let queue_check_beam_h_star sif args = 
    gen2_beam_search 
      ~analysis_mode:true
      ~analysis_type:(Some H_order)
      sif args

let queue_check_beam_print_best_f sif args = 
    gen2_beam_search 
      ~analysis_mode:true
      ~analysis_type:(Some F_order)
      ~print_best_f:true
      sif args



let queue_check_beam_print_best_h sif args = 
    gen2_beam_search 
      ~analysis_mode:true
      ~analysis_type:(Some F_order)
      ~print_best_h:true
      sif args


(* EOF *)

(*

  beam search

*)
open Search_interface


let fp_delta = 0.000001


let no_record = (fun _ _ -> ())
let no_record1 = (fun _ -> ())

type 'a node = {
  data : 'a; (**)
  cost: float;(**)
  incurred_cost: float;(**)
  mutable indecision: float;(*tracks how much worse this node is relatie to
		      the best looking node at its level.*)
  mutable heap_index: int;(*where this node resides in the heap.*)
  mutable p_unexpanded_children: float;(**)
}


let make_initial initial_state =
  { data= initial_state;
    cost=0.0;
    incurred_cost = 0.0;
    indecision = -1.0;
    heap_index = -1;
    p_unexpanded_children = 0.0;}


let get_f n =
  n.cost +. n.incurred_cost


let h_ordered n1 n2 =
(*
  n1.cost < n2.cost
*)

  if (n1.cost < n2.cost) then true
  else false
(*
  else if (n1.cost > n2.cost) then false
  else if (n1 == n2) then false
  else n1 < n2
*)
;;



let f_ordered n1 n2 =
  let del = ((n1.cost +. n1.incurred_cost) -. (n2.cost
					       +. n2.incurred_cost))
  in
    if((abs_float del) < fp_delta) then
      (
	if((abs_float (n1.cost -. n2.cost)) < fp_delta) then false
	else n1.cost <= n2.cost
      )
    else if(del > 0.) then false
    else true
(*
  if ((n1.cost +. n1.incurred_cost) < (n2.cost +. n2.incurred_cost)) then true
  else false
*)
;;


(*
let print_dynamic g h n =
  Verb.pe Verb.often "(";
  Drn_instance.print_state n.data.Dynamic.state;
  Verb.pe Verb.often " g=%f h=%f f=%f)" g h (g +. h)

let print_drn_state n =
  Verb.pe Verb.often "(x=%d y=%d speed=%d heading=%d g=%f h=%f f=%f) "
    n.data.Dynamic.state.Drn_instance.x
    n.data.Dynamic.state.Drn_instance.y
    n.data.Dynamic.state.Drn_instance.speed
    n.data.Dynamic.state.Drn_instance.heading
    n.incurred_cost
    n.cost
    (n.incurred_cost +. n.cost)
*)

let print_tile_cheap t =
  let t2 = Array.make (Array.length t) 0 in
    for i = 0 to ((Array.length t)-1) do
      t2.(t.(i)) <- i;
    done;
    Verb.pf Verb.always stdout "\n%d %d %d\n" t.(0) t.(1) t.(2);
    Verb.pf Verb.always stdout "%d %d %d" t.(3) t.(4) t.(5);
    Verb.pf Verb.always stdout "\n%d %d %d\n" t.(6) t.(7) t.(8);;


let beam_search ?(queue_record = no_record)
    ?(prune_tracker = no_record1) ?(prune_printer = no_record1) i
    beam_width initial is_goal expand ordered_p better_p hash_compare
    int_key key =
  let node_collection = Htable.create int_key hash_compare 10000 in
  let queue_of_nodes_to_expand = Lq.create (fun n1 n2 -> not (ordered_p n1 n2))
    beam_width
    initial in
  let add_child queue_for_child_nodes node_to_add =
    if (is_goal node_to_add) then
      (Limit.new_incumbent i (Limit.Incumbent (0., node_to_add)))
    else (Limit.incr_gen i;
	  if (Htable.mem node_collection (key node_to_add)) then
	    (Limit.incr_dups i);
	  Htable.replace node_collection (key node_to_add) node_to_add;
	  prune_tracker (node_to_add,(Lq.insert queue_for_child_nodes node_to_add));)
  in
    ignore (Lq.insert queue_of_nodes_to_expand initial);
    Htable.add node_collection (key initial) initial;
    let rec expand_layer nq =
      match i.Limit.incumbent with
	  Limit.Incumbent _ -> ()
	| Limit.Nothing ->
	    (queue_record i nq;
	     prune_printer i;
	     if ((not (Lq.empty_p nq)) &&
		   (not (Limit.halt_p i)))
	     then
	       (
		 let cnode = Lq.peek_first nq in
		 let queue_holding_children_nodes = Lq.create
		   (fun n1 n2 -> not (ordered_p n1 n2))
		   beam_width
 		   initial in
		   if is_goal cnode then
		     Limit.new_incumbent i (Limit.Incumbent (0., cnode))
		   else
		     (
		       for count = 1 to (min beam_width (Lq.count nq))
		       do
			 if(not(Limit.halt_p i)) then
			   (
			     Limit.incr_exp i;
			     let node = Lq.extract_first nq in
			       List.iter (add_child
					    queue_holding_children_nodes)
				 (expand node)
			   )
		       done;
		       expand_layer queue_holding_children_nodes;
		     )
	       )
	    )
    in
      expand_layer queue_of_nodes_to_expand;

      Limit.results5 i


let beam_search_non_prune_closed ?(queue_record = no_record)
    ?(prune_tracker = no_record1) ?(prune_printer = no_record1) i
    beam_width initial is_goal expand ordered_p better_p hash_compare
    int_key key =
  let node_collection = Htable.create int_key hash_compare 10000 in
  let queue_of_nodes_to_expand = Lq.create (fun n1 n2 -> not (ordered_p n1 n2))
    beam_width
    initial in
  let add_child queue_for_child_nodes node_to_add =
    if (is_goal node_to_add) then
      (Limit.new_incumbent i (Limit.Incumbent (0., node_to_add)))
    else (Limit.incr_gen i;
	  if (Htable.mem node_collection (key node_to_add)) then
	    (Limit.incr_dups i);
	  Htable.replace node_collection (key node_to_add) node_to_add;
	  prune_tracker (node_to_add,(Lq.insert queue_for_child_nodes node_to_add));)
  in
    ignore (Lq.insert queue_of_nodes_to_expand initial);
    Htable.add node_collection (key initial) initial;
    let rec expand_layer nq =
      match i.Limit.incumbent with
	  Limit.Incumbent _ -> ()
	| Limit.Nothing ->
	    (queue_record i nq;
	     prune_printer i;
	     if ((not (Lq.empty_p nq)) &&
		   (not (Limit.halt_p i)))
	     then
	       (
		 let cnode = Lq.peek_first nq in
		 let queue_holding_children_nodes = Lq.create
		   (fun n1 n2 -> not (ordered_p n1 n2))
		   beam_width
 		   initial in
		   if is_goal cnode then
		     Limit.new_incumbent i (Limit.Incumbent (0., cnode))
		   else
		     (
		       for count = 1 to (min beam_width (Lq.count nq))
		       do
			 if(not(Limit.halt_p i)) then
			   (
			     Limit.incr_exp i;
			     let node = Lq.extract_first nq in
			       List.iter (add_child
					    queue_holding_children_nodes)
				 (expand node)
			   )
		       done;
		       expand_layer queue_holding_children_nodes;
		     )
	       )
	    )
    in
      expand_layer queue_of_nodes_to_expand;
      Limit.results6 i



let beam_search_dups ?(queue_record = no_record)
    ?(prune_tracker = no_record1) ?(prune_printer = no_record1) i
    beam_width initial is_goal expand ordered_p better_p hash_compare
    int_key key node_printer=
  let queue_of_nodes_to_expand = Lq.create
    ~update_function:(fun n index ->
			n.heap_index <- index)
    (fun n1 n2 -> not (ordered_p n1 n2))
    beam_width
    initial in
  let ht = Htable.create int_key hash_compare 10000 in
  let add_child queue_to_stick_children_in child =
    if (is_goal child) then (Limit.new_incumbent i
			       (Limit.Incumbent (0., child)))
    else if(i.Limit.incumbent != Limit.Nothing) then ()
    else if(Limit.halt_p i) then ()

    (* node is in the hash table, but we will give it one more chance, if
       the latest expanstion is better, allow it to compete for entry into
       the beam.
       1) increment the number of duplicate nodes encountered, since
       this node is a duplicate.
       2) Check to see if this node is better than the previous
       version of it.
       3) if it is better, consider putting it into the beam.
       3a) replace node in the hash table, since this new one is better.
       3b) If the node was already in the queue, replace it in the
       queue
       3c) otherwise just insert node into queue
       3d) if some node is knocked out of the queue, take the node
       out of the hash table as well, but only do this if the node
       is a brand new node.
    *)
    else if(Htable.mem ht (key child)) then
      (Limit.incr_dups i;
       let old_node = Htable.find ht (key child) in
	 if(old_node.incurred_cost > child.incurred_cost) then
	   (
	     let old_index = old_node.heap_index in
	       Htable.replace ht (key child) child;
	       if(old_index != -1) then
		 List.iter (fun n ->
			      (prune_tracker (child,(n));
			       if(n.heap_index != -1) then
				 (
				   Htable.remove ht (key n);
				 )
			      )
			   )
		   (Lq.insert_index queue_to_stick_children_in
		      old_index child;)
	       else
		 (List.iter
		    (fun n ->
		       (prune_tracker (child,(n));
			if(n.heap_index != -1) then
			  (
			    Htable.remove ht (key n);
			  )
		       )
		    )
		    (Lq.insert queue_to_stick_children_in child;)
		 )))
	(*
	  node was not in the hash table.
	  1) add node to the hash table
	  2) insert node into queue
	  3) if some node is knocked out of the beam, take that node
	  out of the hash table as well.
	*)
    else (
      Limit.incr_gen i;
      Htable.replace ht (key child) child;
      List.iter

	(fun n ->
	   prune_tracker (child,(n));
	   (*
	     Verb.pe Verb.always "pruned this: ";
	     print_drn_state n;
	     Verb.pe Verb.always "\n";
	   *)
	   if(n.heap_index != -1) then Htable.remove ht (key n);)
	(Lq.insert queue_to_stick_children_in child;)
    ) in
    ignore (Lq.insert queue_of_nodes_to_expand initial);
    Htable.add ht (key initial) initial;
    let rec expand_layer nq =
      (*
	Printf.fprintf stderr "beam\n";
	Lq.print_things nq (node_printer) "\n";
	Printf.fprintf stderr "beam\n";
	flush stderr;
      *)
      if (i.Limit.incumbent != Limit.Nothing) then ()
      else
	(
	  (*
	    Verb.pe Verb.often "%i " i.Limit.expanded;
	    Lq.unsafe_iter nq print_drn_state;
	    Verb.pe Verb.often "\n";
	  *)
	  queue_record i nq;
	  prune_printer i;
	  if ((not (Lq.empty_p nq)) &&
		(not (Limit.halt_p i)))
	  then
	    (
	      let cnode = Lq.peek_first nq in
	      let queue_holding_children_nodes = Lq.create
		~update_function:(fun n index ->
				    n.heap_index <- index)
		(fun n1 n2 -> not (ordered_p n1 n2))
		beam_width
 		initial in
		if is_goal cnode then
		  Limit.new_incumbent i (Limit.Incumbent (0., cnode))
		else
		  (
		    for count = 1 to (min beam_width (Lq.count nq))
		    do
		      if(not(Limit.halt_p i) && i.Limit.incumbent = Limit.Nothing) then
			(
			  Limit.incr_exp i;
 			  List.iter (add_child
				       queue_holding_children_nodes)
			    (expand (Lq.extract_first nq));
			)
		    done;
		    expand_layer queue_holding_children_nodes;
		  )
	    )
	)
    in
      expand_layer queue_of_nodes_to_expand;
      Limit.results6 i


let militant_beam_search_dups ?(queue_record = no_record)
    ?(prune_tracker = no_record1) ?(prune_printer = no_record1) i
    beam_width initial is_goal expand ordered_p better_p hash_compare
    int_key key=
  let queue_of_nodes_to_expand = Lq.create
    ~update_function:(fun n index ->
			n.heap_index <- index)
    (fun n1 n2 -> not (ordered_p n1 n2))
    beam_width
    initial in
  let ht = Htable.create int_key hash_compare 10000 in
  let add_child queue_to_stick_children_in child =
    if (is_goal child) then (Limit.new_incumbent i
			       (Limit.Incumbent (0., child)))
      (* node is in the hash table, but this militant version of beam
	 search doesn't really care so it just kills this child.
      *)
    else if(Htable.mem ht (key child)) then
      (Limit.incr_dups i;)
	(*
	  node was not in the hash table.
	  1) add node to the hash table
	  2) insert node into queue
	  3) if some node is knocked out of the beam, take that node
	  out of the hash table as well.
	*)
    else (
      Limit.incr_gen i;
      Htable.replace ht (key child) child;
      List.iter (fun n ->
	prune_tracker (child,(Some n));
	if(n.heap_index != -1) then Htable.remove ht (key n);)
       (Lq.insert queue_to_stick_children_in child;)
    ) in
    ignore (Lq.insert queue_of_nodes_to_expand initial);
    Htable.add ht (key initial) initial;
    let rec expand_layer nq =
      if (i.Limit.incumbent != Limit.Nothing) then ()
      else
	(
	  queue_record i nq;
	  prune_printer i;
	  if ((not (Lq.empty_p nq)) &&
		(not (Limit.halt_p i)))
	  then
	    (
	      let cnode = Lq.peek_first nq in
	      let queue_holding_children_nodes = Lq.create
		~update_function:(fun n index ->
				    n.heap_index <- index)
		(fun n1 n2 -> not (ordered_p n1 n2))
		beam_width
 		initial in
		if is_goal cnode then
		  Limit.new_incumbent i (Limit.Incumbent (0., cnode))
		else
		  (
		    for count = 1 to (min beam_width (Lq.count nq))
		    do
		      if(not(Limit.halt_p i)) then
			(
			  Limit.incr_exp i;
 			  List.iter (add_child
				       queue_holding_children_nodes)
			    (expand (Lq.extract_first nq) );
			)
		    done;
		    expand_layer queue_holding_children_nodes;
		  )
	    )
	)
    in
      expand_layer queue_of_nodes_to_expand;
      Limit.results6 i



let restarting_beam_search_dups ?(queue_record = no_record)
    ?(prune_tracker = no_record1) ?(prune_printer = no_record1) args i
    beam_width initial is_goal expand ordered_p better_p hash_compare
    int_key key h=
  let calc_indecision =
    match (Search_args.get_string "ib_beam_dups" args 1) with
	"indecision" -> true
      | _ -> false in
  let backtrack_count = ref 0 in
  let prune_ht = Htable.create int_key hash_compare 10000 in
  let prune_list = ref [] in
  let process_prune n = (
    Htable.replace prune_ht (key n) n;
    if(calc_indecision) then prune_list := n :: !prune_list;
    n.heap_index <- (-1);
  ) in
  let queue_of_nodes_to_expand = Picky_queue.create
    ~update_function:(fun n index ->
			n.heap_index <- index)
    ~pickiness:(Some 2.0)
    (fun n1 n2 -> (ordered_p n1 n2))
    get_f
    beam_width
    initial in
  let ht = Htable.create int_key hash_compare 10000 in
  let check_ht () = Htable.iter (fun _ n -> if(n.heap_index != (-1)) then
				   failwith "hash table corruption";)
    ht;
    Printf.fprintf stderr "hash table ok\n" in
  let p_new_kids n =
    let all_descendents = List.rev_append (expand n)
      (List.flatten (List.rev_map expand (expand n))) in
    let new_nodes = ref 0. in
    let all_nodes = ref 0.00001 in
      List.iter (fun f ->
		   all_nodes := !all_nodes +. 1.0;
		   if(Htable.mem ht (key f)) then
		     new_nodes := !new_nodes +. 1.0;)
	all_descendents;
      if(!new_nodes = 0.) then 0.0000001
      else
	!new_nodes /. !all_nodes
  in
  let secondary_sort_predicate n1 n2 =
    match (Search_args.get_string "ib_beam_dups" args 1) with
	"indecision"-> (n1.indecision < n2.indecision)
      | "h"->h_ordered n1 n2
      | "f"->f_ordered n1 n2
	  (* low borderness is good *)
      | "border"-> (((p_new_kids n1)) < ((p_new_kids n2)))
      | "h_border_hybrid"-> (((p_new_kids n1) *. n1.cost) <
			       ((p_new_kids n2) *. n2.cost))
      | "f_border_hybrid"-> (((p_new_kids n1) *. (n1.cost +. n1.incurred_cost)) <
			       ((p_new_kids n2) *. (n2.cost +. n1.incurred_cost)))
	  (* high borderness is good *)
      | "rev_border"-> (((p_new_kids n1)) > ((p_new_kids n2)))
      | "rev_h_border_hybrid"-> ((n1.cost /. (p_new_kids n1)) <
				   (n2.cost /. (p_new_kids n2)))
      | "rev_f_border_hybrid"-> (((n1.cost +. n1.incurred_cost) /. (p_new_kids n1) *. (p_new_kids n1)) <
				   ((n2.cost +. n1.incurred_cost) /. (p_new_kids n2)))
      | _ -> failwith "invalid secondary sort predicate";  in


  let add_child queue_to_stick_children_in child =
    if (is_goal child) then (Limit.new_incumbent i
			       (Limit.Incumbent (0., child)))
    else if(i.Limit.incumbent != Limit.Nothing) then ()
    else if(Limit.halt_p i) then ()
      (* node is in the hash table, but we will give it one more chance, if
	 the latest expanstion is better, allow it to compete for entry into
	 the beam.
	 1) increment the number of duplicate nodes encountered, since
	 this node is a duplicate.
	 2) Check to see if this node is better than the previous
	 version of it.
	 3) if it is better, consider putting it into the beam.
	 3a) replace node in the hash table, since this new one is better.
	 3b) If the node was already in the queue, replace it in the
	 queue
	 3c) otherwise just insert node into queue
	 3d) if some node is knocked out of the queue, take the node
	 out of the hash table as well, but only do this if the node
	 is a brand new node.
      *)
    else if(Htable.mem ht (key child)) then
      (Limit.incr_dups i;
       let old_node = Htable.find ht (key child) in
	 if(old_node.incurred_cost > child.incurred_cost) then
	   (
	     let old_index = old_node.heap_index in
	       Htable.replace ht (key child) child;
	       if(old_index != -1) then
		 (
		   Printf.fprintf stderr "%d %d old index\n" old_index
		     (Picky_queue.count queue_to_stick_children_in);
		   let pnl = Picky_queue.insert_index queue_to_stick_children_in
		     old_index child in
		     List.iter (
		       fun n ->
			 prune_tracker (child, Some n);
			 process_prune n;
			 if(n.heap_index != -1) then
			   (
			     Htable.remove ht (key n);
			   )
		     ) pnl;
		 )
	       else
		 (match (
		    Picky_queue.insert queue_to_stick_children_in child;
		  )
		  with
		      [] -> ();
		    | a -> List.iter (fun n ->
					prune_tracker (child, Some n);
					process_prune n;
					if(n.heap_index != -1) then
					  (
					    Htable.remove ht (key n);
					  )
				     ) a;
		 )
	   )
      )
	(*
	  node was not in the hash table.
	  1) add node to the hash table
	  2) insert node into queue
	  3) if some node is knocked out of the beam, take that node
	  out of the hash table as well.
	*)
    else (
      Limit.incr_gen i;
      Htable.replace ht (key child) child;
      match (Picky_queue.insert queue_to_stick_children_in child;)
      with
	  [] -> ();
	| a -> List.iter (fun n ->
			    prune_tracker (child, Some n);
			    process_prune n;
			    if(n.heap_index != -1) then
			      (
				Htable.remove ht (key n);
			      )
			 ) a;
    ) in
    ignore (Picky_queue.insert queue_of_nodes_to_expand initial);
    Htable.add ht (key initial) initial;
    let rec expand_layer nq =
      if (i.Limit.incumbent != Limit.Nothing) then ()
      else if(Picky_queue.empty_p nq) then ()
      else
	(
	  (*
	    assert ((get_f (Picky_queue.peek_first nq)) <=
	    (get_f (Picky_queue.peek_largest nq)));
	  *)

	  prune_list := [];
	  queue_record i nq;
	  prune_printer i;
	  if ((not (Picky_queue.empty_p nq)) &&
		(not (Limit.halt_p i)))
	  then
	    (
	      let cnode = Picky_queue.peek_first nq in
	      let queue_holding_children_nodes = Picky_queue.create
		~update_function:(fun n index ->
				    n.heap_index <- index)
		(fun n1 n2 -> (ordered_p n1 n2))
		~pickiness:(Some 2.0)
		get_f
		beam_width
 		initial in
		if is_goal cnode then
		  Limit.new_incumbent i (Limit.Incumbent (0., cnode))
		else
		  (
		    for count = 1 to (min beam_width (Picky_queue.count nq))
		    do
		      if(i.Limit.incumbent != Limit.Nothing) then
			(
			  ignore(Picky_queue.extract_first nq);
			)
		      else if(not(Limit.halt_p i)) then
			(
			  Limit.incr_exp i;
			  let node_being_expanded = (Picky_queue.extract_first nq) in
 			    List.iter (add_child queue_holding_children_nodes)
			      (expand node_being_expanded);
			    Htable.remove prune_ht (key node_being_expanded);
			)
		    done;
		    (*now we have the children nodes, and can set the
		      indecision, if required.*)
		    if(not(Picky_queue.empty_p queue_holding_children_nodes)) then
		      (
			let best_f = get_f (Picky_queue.peek_first
					      queue_holding_children_nodes) in
			  List.iter (fun n -> n.indecision <- (get_f n)
				       -. best_f) !prune_list;
		      );
		    expand_layer queue_holding_children_nodes;
		  )
	    )
	)
    in
      expand_layer queue_of_nodes_to_expand;
      (* this is supposed to take the hash table of inadmissably
	 pruned nodes and make a new beam of the best ones.  Best is
	 defined by proximity to the edge of the search space as well
	 as quality. *)
      while(i.Limit.incumbent = Limit.Nothing && not(Limit.halt_p i))
      do
	check_ht ();
	backtrack_count := !backtrack_count + 1;
	let new_beam = Picky_queue.create
	  ~update_function:(fun n index ->  n.heap_index <- index)
          (fun n1 n2 -> (secondary_sort_predicate n1 n2))
	  ~pickiness:(Some 2.0)
	  get_f
	  beam_width initial in
	  Htable.iter (fun _ node ->
			 Picky_queue.insert new_beam node;
		      )
	    prune_ht;
	  expand_layer new_beam;
      done;
      Datafile.write_pairs stdout ["backtracks",(string_of_int !backtrack_count)];
      Limit.results6 i



let beam_search_queue_dups
    ?(queue_record = no_record)
    ?(prune_tracker = no_record1)
    ?(prune_printer = no_record1)
    i beam_width initial is_goal expand ordered_p better_p hash_compare
    int_key key=
  let queue_of_nodes_to_expand = Lq.create
    ~update_function:(fun n index ->
			n.heap_index <- index)
    (fun n1 n2 -> not (ordered_p n1 n2))
    beam_width
    initial in
  let add_child queue_to_stick_children_in ht child =
    if (is_goal child) then (Limit.new_incumbent i
			       (Limit.Incumbent (0., child)))
      (* node is in the hash table, but we will give it one more chance, if
	 the latest expanstion is better, allow it to compete for entry into
	 the beam.
	 1) increment the number of duplicate nodes encountered, since
	 this node is a duplicate.
	 2) Check to see if this node is better than the previous
	 version of it.
	 3) if it is better, consider putting it into the beam.
	 3a) replace node in the hash table, since this new one is better.
	 3b) If the node was already in the queue, replace it in the
	 queue
	 3c) otherwise just insert node into queue
	 3d) if some node is knocked out of the queue, take the node
	 out of the hash table as well, but only do this if the node
	 is a brand new node.
      *)
    else if(Htable.mem ht (key child)) then
      (Limit.incr_dups i;
       let old_node = Htable.find ht (key child) in
	 if(old_node.incurred_cost > child.incurred_cost) then
	   (
	     let old_index = old_node.heap_index in
	       Htable.replace ht (key child) child;
	       if(old_index != -1) then
		 List.iter
		   (fun n ->
		      (prune_tracker (child,(n));
		       if(n.heap_index != -1) then
			 (
			   Htable.remove ht (key n);
			 )
		      ))
		   (Lq.insert_index queue_to_stick_children_in
		      old_index child;)
	       else
		 (
		   List.iter
		     (fun n ->
			(prune_tracker (child,(n));
			 if(n.heap_index != -1) then
			   (
			     Htable.remove ht (key n);
			   )
			))
		     (
		       Lq.insert queue_to_stick_children_in child;
		     )

		 )
	   )
      )
	(*
	  node was not in the hash table.
	  1) add node to the hash table
	  2) insert node into queue
	  3) if some node is knocked out of the beam, take that node
	  out of the hash table as well.
	*)
    else (
      Limit.incr_gen i;
      Htable.replace ht (key child) child;
      List.iter (fun n ->
	prune_tracker (child,(n));
	if(n.heap_index != -1) then Htable.remove ht (key n);)
	  (Lq.insert queue_to_stick_children_in child;)

    ) in
    ignore (Lq.insert queue_of_nodes_to_expand initial);
    let rec expand_layer nq =
      if (i.Limit.incumbent != Limit.Nothing) then ()
      else
	(
	  queue_record i nq;
	  prune_printer i;
	  if ((not (Lq.empty_p nq)) &&
		(not (Limit.halt_p i)))
	  then
	    (
	      let cnode = Lq.peek_first nq in
	      let ht = Htable.create int_key hash_compare (beam_width
							   * 2) in
	      let queue_holding_children_nodes = Lq.create
		~update_function:(fun n index ->
				    n.heap_index <- index)
		(fun n1 n2 -> not (ordered_p n1 n2))
		beam_width
 		initial in
		if is_goal cnode then
		  Limit.new_incumbent i (Limit.Incumbent (0., cnode))
		else
		  (
		    for count = 1 to (min beam_width (Lq.count nq))
		    do
		      if(not(Limit.halt_p i)) then
			(
			  Limit.incr_exp i;
 			  List.iter (add_child
				       queue_holding_children_nodes ht)
			    (expand (Lq.extract_first nq) );
			)
		    done;
		    expand_layer queue_holding_children_nodes;
		  )
	    )
	)
    in
      expand_layer queue_of_nodes_to_expand;
      Limit.results6 i



let bf_beam_search ?(queue_record = no_record)
    ?(prune_tracker = no_record1) ?(prune_printer = no_record1) i
    beam_width initial is_goal expand ordered_p better_p hash_compare
    int_key key=
  let queue_of_nodes_to_expand = Mmh.create (fun n1 n2 -> (ordered_p n1 n2))
    beam_width
    initial in
    ignore (Mmh.insert queue_of_nodes_to_expand initial);
    let rec expand_layer ()=
      match i.Limit.incumbent with
	  Limit.Incumbent _ -> ()
	| Limit.Nothing ->
	    (
	      queue_record i queue_of_nodes_to_expand;
	      prune_printer i;
	      if ((not (Mmh.empty_p queue_of_nodes_to_expand)) &&
		    (not (Limit.halt_p i)))
	      then
		(
		  let cnode = Mmh.peek_first queue_of_nodes_to_expand in
		    if is_goal cnode then
		      Limit.new_incumbent i (Limit.Incumbent (0., cnode))
		    else
		      (Limit.incr_exp i;
		       let node = Mmh.extract_first
			 queue_of_nodes_to_expand in

		       let rec add_child child_list = match child_list with
			   [] -> ()
			 | hd::tl -> (
			     if (is_goal hd) then
			       (Limit.new_incumbent i
				  (Limit.Incumbent (0., hd)))
			     else (Limit.incr_gen i;
				   ignore (Mmh.insert
					     queue_of_nodes_to_expand hd);
				   add_child tl;)
			   ) in
			 add_child (expand node);
			 expand_layer ();
		      )
		)
	    )
    in
      expand_layer ();
      Limit.results5 i


let bf_beam_search_dups ?(queue_record = no_record)
    ?(prune_tracker = no_record1) ?(prune_printer = no_record1)
    i beam_width initial is_goal expand ordered_p better_p hash_compare
    int_key key =
  let prune_same_level = ref 0 in
  let prune_deeper_replacement = ref 0 in
  let prune_shallow_replacement = ref 0 in
  let real_prune_tracker prune_pair = prune_tracker prune_pair;
    let newnode, victim = prune_pair in
      match victim with
	  Some n -> (
	    if(n.incurred_cost = newnode.incurred_cost) then
	      prune_same_level := !prune_same_level + 1
	    else if (n.incurred_cost < newnode.incurred_cost) then
	      prune_deeper_replacement := !prune_deeper_replacement + 1
	    else
	      prune_shallow_replacement := !prune_shallow_replacement + 1)
	| None -> failwith "Can not be 'None'"
  in

  let queue_of_nodes_to_expand = Mmh.create (fun n1 n2 -> (ordered_p n1 n2))
    beam_width
    initial in
  let ht = Htable.create int_key hash_compare 10000 in
    ignore (Mmh.insert queue_of_nodes_to_expand initial);
    Htable.add ht (key initial) initial;
    let rec expand_layer ()=
      match i.Limit.incumbent with
	  Limit.Incumbent _ -> ()
	| Limit.Nothing ->
	    (
	      queue_record i queue_of_nodes_to_expand;
	      prune_printer i;
	      if ((not (Mmh.empty_p queue_of_nodes_to_expand)) &&
		    (not (Limit.halt_p i)))
	      then
		(
		  let cnode = Mmh.peek_first queue_of_nodes_to_expand in
		    if is_goal cnode then
		      Limit.new_incumbent i (Limit.Incumbent (0., cnode))
		    else
		      (Limit.incr_exp i;
		       let node = Mmh.extract_first
			 queue_of_nodes_to_expand in

		       let rec add_child child_list = match child_list with
			   [] -> ()
			 | hd::tl -> (
			     if (is_goal hd) then
			       (Limit.new_incumbent i
				  (Limit.Incumbent (0., hd)))
			     else if (Htable.mem ht (key hd)) then
			       (
				 Limit.incr_dups i;
				 add_child tl;
			       )
			     else (Limit.incr_gen i;
				   Htable.add ht (key hd) hd;
				   let pruned = Mmh.insert
				     queue_of_nodes_to_expand
				     hd in
				     List.iter (fun n ->
						  (
						    real_prune_tracker (hd,(Some n));
						    Htable.remove ht (key n);
						  )) pruned;
				     add_child tl;)
			   ) in
			 add_child (expand node);
			 expand_layer ();
		      )
		)
	    )
    in
      expand_layer ();

      Datafile.write_pairs stdout
	["same level",(string_of_int !prune_same_level)];
      Datafile.write_pairs stdout
	["deeper replacement",(string_of_int !prune_deeper_replacement)];
      Datafile.write_pairs stdout
	["shallower replacement",(string_of_int !prune_shallow_replacement)];

      Limit.results6 i




let wrap_expand ?(weight=1.0) expand h record =
  (fun n -> List.map (fun (d, g) -> {data = d;
				     cost = ((h d) *. weight);
				     incurred_cost= g;
				     indecision = -1.0;
			  heap_index = -1;
			  p_unexpanded_children = 0.0;
				    }
		     ) (expand n.data n.incurred_cost)
  )


let wrap f =
  (fun n -> f n.data)


let wrap2 f =
  (fun a b -> f a.data b.data)


let unwrap_sol_node s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.incurred_cost)


let call_beam_search sif args =
  let beam_width = Search_args.get_int "Beam.call_beam_search" args 0 in
  let initial_node =   (make_initial sif.initial) in
  let solution =
    beam_search
      (Limit.make Limit.Nothing sif.halt_on h_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width
      initial_node
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      h_ordered
      h_ordered
      sif.equals
      sif.hash
      (wrap sif.key) in
    Limit.unwrap_sol5 unwrap_sol_node solution


let call_beam_search_no_closed sif args =
  let beam_width = Search_args.get_int "Beam.call_beam_search" args 0 in
  let initial_node = (make_initial sif.initial) in
  let solution =
    beam_search
      (Limit.make Limit.Nothing sif.halt_on h_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width
      initial_node
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      h_ordered
      h_ordered
      sif.equals
      sif.hash
      (wrap sif.key) in
    Limit.unwrap_sol5_into_6 unwrap_sol_node solution


let call_f_beam_search_no_closed sif args =
  let beam_width = Search_args.get_int
  "Beam.call_f_beam_search_no_closed"
  args 0 in
  let initial_node = (make_initial sif.initial) in
  let solution =
    beam_search
      (Limit.make Limit.Nothing sif.halt_on f_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width
      initial_node
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      f_ordered
      f_ordered
      sif.equals
      sif.hash
      (wrap sif.key) in
    Limit.unwrap_sol5_into_6 unwrap_sol_node solution


let call_f_beam_search_ghost_closed sif args =

  let beam_width = Search_args.get_int
  "Beam.call_f_beam_search_ghost_closed"
  args 0 in
  let initial_node =   (make_initial sif.initial) in
  let solution =
    beam_search_non_prune_closed
      (Limit.make Limit.Nothing sif.halt_on f_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width
      initial_node
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      f_ordered
      f_ordered
      sif.equals
      sif.hash
      (wrap sif.key) in
    Limit.unwrap_sol6 unwrap_sol_node solution




let call_beam_search_dups sif args =
  let beam_width = Search_args.get_int "Beam.call_beam_search_dups" args 0 in
    Limit.unwrap_sol6 unwrap_sol_node (
      beam_search_dups
	(Limit.make Limit.Nothing sif.halt_on h_ordered
	   (Limit.make_default_logger (fun n -> n.incurred_cost)
	      (wrap sif.get_sol_length)))
	beam_width (make_initial sif.initial)
	(wrap sif.goal_p)
	(wrap_expand sif.domain_expand sif.h no_record)
	h_ordered
	h_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
	(fun a -> String.concat " "
	   [(sif.key_printer (wrap sif.key a));
	    "f value:";
	    (string_of_float a.incurred_cost);
	    "h value:";
	    (string_of_float a.cost);
	   ]
	)
    )


let call_militant_f_beam_search_dups sif args =
  let beam_width = Search_args.get_int "Beam.call_militant_f_beam_search_dups"
    args 0 in
  Limit.unwrap_sol6 unwrap_sol_node (
    militant_beam_search_dups
      (Limit.make Limit.Nothing sif.halt_on h_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width
      (make_initial sif.initial)
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      f_ordered
      f_ordered
      sif.equals
      sif.hash
      (wrap sif.key)
  )



let call_restarting_beam_search_dups sif args =
  let beam_width = Search_args.get_int "Beam.call_restarting_beam_search_dups"
    args 0 in
    Limit.unwrap_sol6 unwrap_sol_node (
      restarting_beam_search_dups
	args
	(Limit.make Limit.Nothing sif.halt_on h_ordered
	   (Limit.make_default_logger (fun n -> n.incurred_cost)
	      (wrap sif.get_sol_length)))
	beam_width
	(make_initial sif.initial)
	(wrap sif.goal_p)
	(wrap_expand sif.domain_expand sif.h no_record)
	f_ordered
	f_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
	(wrap sif.h)
    )


let call_f_beam_search_dups sif args =
  Printf.fprintf stdout "beam search got called with %d arguments\n"
    (Array.length args);
  Wrarray.print_array_index stdout args;
  flush stdout;

  let beam_width = Search_args.get_int "Beam.call_f_beam_search_dups" args 0 in
    Limit.unwrap_sol6 unwrap_sol_node (
      beam_search_dups
	(Limit.make Limit.Nothing sif.halt_on f_ordered
	   (Limit.make_default_logger (fun n -> n.incurred_cost)
	      (wrap sif.get_sol_length)))
	beam_width (make_initial sif.initial)
	(wrap sif.goal_p)
	(wrap_expand sif.domain_expand sif.h no_record)
	f_ordered
	f_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
	(fun a -> sif.key_printer (wrap sif.key a))
    )


let call_f_beam_search_queue_dups sif args =
  let beam_width = Search_args.get_int "Beam.call_f_beam_search_queue_dups"
    args 0 in
  Limit.unwrap_sol6 unwrap_sol_node (
    beam_search_queue_dups
      (Limit.make Limit.Nothing sif.halt_on f_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width (make_initial sif.initial)
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      f_ordered
      f_ordered
      sif.equals
      sif.hash
      (wrap sif.key)
  )



let call_bf_beam_search sif args =
  let beam_width = Search_args.get_int "Beam.call_bf_beam_search" args 0 in
  Limit.unwrap_sol5 unwrap_sol_node (

    bf_beam_search
      (Limit.make Limit.Nothing sif.halt_on h_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width (make_initial sif.initial)
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      h_ordered
      h_ordered
      sif.equals
      sif.hash
      (wrap sif.key)
  )

let call_bf_beam_search_no_closed sif args =
  let beam_width = Search_args.get_int
  "Beam.call_bf_beam_search_no_closed" args 0 in
  Limit.unwrap_sol5_into_6 unwrap_sol_node (

    bf_beam_search
      (Limit.make Limit.Nothing sif.halt_on h_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width (make_initial sif.initial)
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      h_ordered
      h_ordered
      sif.equals
      sif.hash
      (wrap sif.key)
  )


let call_bf_beam_search_dups sif args =
  let beam_width = Search_args.get_int "Beam.call_bf_beam_search_dups"
    args 0 in
  Limit.unwrap_sol6 unwrap_sol_node (

    bf_beam_search_dups
      (Limit.make Limit.Nothing sif.halt_on h_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width (make_initial sif.initial)
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      h_ordered
      h_ordered
      sif.equals
      sif.hash
      (wrap sif.key)
  )


let call_f_bf_beam_search_dups sif args =
  let beam_width = Search_args.get_int "Beam.call_f_bf_beam_search_dups"
    args 0 in

  Limit.unwrap_sol6 unwrap_sol_node (

    bf_beam_search_dups
      (Limit.make Limit.Nothing sif.halt_on f_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width (make_initial sif.initial)
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      f_ordered
      f_ordered
      sif.equals
      sif.hash
      (wrap sif.key)
  )


let call_wted_f_bf_beam_search_dups sif args =
  let beam_width = Search_args.get_int "Beam.call_f_bf_beam_search_dups"
    args 0 in
  let wt = Search_args.get_float "Beam.call_f_bf_beam_search_dups"
    args 1 in

  Limit.unwrap_sol6 unwrap_sol_node (

    bf_beam_search_dups
      (Limit.make Limit.Nothing sif.halt_on f_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width (make_initial sif.initial)
      (wrap sif.goal_p)
      (wrap_expand ~weight:wt sif.domain_expand sif.h no_record)
      f_ordered
      f_ordered
      sif.equals
      sif.hash
      (wrap sif.key)
  )




let call_f_bf_beam_search sif args =
  let beam_width = Search_args.get_int "Beam.call_f_bf_beam_search"
    args 0 in
  Limit.unwrap_sol5 unwrap_sol_node (

    bf_beam_search
      (Limit.make Limit.Nothing sif.halt_on f_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width (make_initial sif.initial)
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      f_ordered
      f_ordered
      sif.equals
      sif.hash
      (wrap sif.key)
  )


let call_f_bf_beam_search_no_closed sif args =
  let beam_width = Search_args.get_int "Beam.call_f_bf_beam_search"
    args 0 in
    Limit.unwrap_sol5_into_6 unwrap_sol_node (

      bf_beam_search
	(Limit.make Limit.Nothing sif.halt_on f_ordered
	   (Limit.make_default_logger (fun n -> n.incurred_cost)
	      (wrap sif.get_sol_length)))
	beam_width (make_initial sif.initial)
	(wrap sif.goal_p)
	(wrap_expand sif.domain_expand sif.h no_record)
	f_ordered
	f_ordered
	sif.equals
	sif.hash
	(wrap sif.key)
    )

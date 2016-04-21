open Search_interface

let no_record = (fun _ _ -> ())
let no_record1 = (fun _ -> ())

let fp_compare_delta = 0.00001


type 'a node = {
  data : 'a;
  mutable cost: float;
  mutable incurred_cost: float;
  mutable heap1_index: int;
  mutable heap2_index: int;
}

let update_index1 n i = 
  n.heap1_index <- i
let update_index2 n i = 
  n.heap2_index <- i

let get_index1 n = 
  n.heap1_index
let get_index2 n = 
  n.heap2_index


let h_ordered n1 n2 =
  let delta_node = n1.cost -. n2.cost in
    if((abs_float delta_node) < fp_compare_delta) 
    then n1.incurred_cost < n2.incurred_cost
    else if (delta_node > 0.0)
    then false
    else true

let f_ordered n1 n2 =
  let delta_node = (n1.cost +. n1.incurred_cost) -. 
    (n2.cost +. n2.incurred_cost) in
    if((abs_float delta_node) < fp_compare_delta)
    then n1.incurred_cost > n2.incurred_cost
    else if (delta_node > 0.0)
    then false
    else true


let fhp_beam_search_dups ?(queue_record = no_record)
    ?(prune_tracker = no_record1) ?(prune_printer = no_record1) i
    beam_width initial is_goal expand ordered_p better_p
    prune_criteria hash_compare
    int_key key=
  let queue_of_nodes_to_expand = Dq.create 
    (fun n1 n2 -> (ordered_p n1 n2))
    (fun n1 n2 -> (prune_criteria n2 n1))
    update_index1
    update_index2
    get_index1
    get_index2
    beam_width
    initial in
  let ht = Htable.create int_key hash_compare 10000 in
    ignore (Dq.insert queue_of_nodes_to_expand initial);
    Htable.add ht (key initial) initial;
    let rec expand_layer ()=
      match i.Limit.incumbent with
	  Limit.Incumbent _ -> ()
	| Limit.Nothing ->
	    (
	      queue_record i queue_of_nodes_to_expand;
	      prune_printer i;
	      if ((not (Dq.empty_p queue_of_nodes_to_expand)) &&
		    (not (Limit.halt_p i)))
	      then
		(
		  let cnode = Dq.peek_first queue_of_nodes_to_expand in
		    if is_goal cnode then
		      Limit.new_incumbent i (Limit.Incumbent (0., cnode))
		    else
		      (Limit.incr_exp i;
		       let node = Dq.extract_first
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
				   match (Dq.insert
					    queue_of_nodes_to_expand
					    hd)
				   with Some n -> Htable.remove ht (key n);
				     | None -> ();

					 add_child tl;)
			   ) in
			 add_child (expand node);
			 expand_layer ();
		      )
		)
	    )
    in
      expand_layer ();
      Limit.results6 i


let wrap_expand expand h record =
  (fun n -> List.map (fun (d, g) -> {data = d;
				     cost = (h d);
				     incurred_cost= g;
				     heap1_index = -1;
				     heap2_index = -1
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


let call_fhp_beam_search_dups sif args =
  let beam_width = Search_args.get_int "Fhp_beam.call_fhp_beam_search_dups"
    args 0 in
  Limit.unwrap_sol6 unwrap_sol_node (

    fhp_beam_search_dups
      (Limit.make Limit.Nothing sif.halt_on f_ordered
	 (Limit.make_default_logger (fun n -> n.incurred_cost)
	    (wrap sif.get_sol_length)))
      beam_width {data=sif.initial;
		  cost=0.0;
		  incurred_cost = 0.0;
		  heap1_index = -1;
		  heap2_index = -1;
		 }
      (wrap sif.goal_p)
      (wrap_expand sif.domain_expand sif.h no_record)
      f_ordered
      f_ordered
      h_ordered 
      sif.equals
      sif.hash
      (wrap sif.key)
  )

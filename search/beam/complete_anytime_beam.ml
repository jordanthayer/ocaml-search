(**

   Does a complete anytime beam search.  

*)

type 'a cab_node =
{
  data: 'a;
  f: float;
  g: float;
}


let make_initial initial_state initial_h = 
  {
    data = initial_state;
    f = initial_h;
    g = 0.0;
  }

let record_wrap_expand 
    expand 
    h 
    (weight:float) 
    =
  (fun n ->
     let children = (List.map (fun (d, g) ->
                                 { data = d;
                                   f = ((h d) *. weight) +. g;
                                   g = g;})
                       (expand n.data n.g)) in
       children)

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



let h_prune delta parent child = 
  let p_h = parent.f -. parent.g in
  let c_h = child.f -. child.g in
    c_h > p_h +. delta


let f_prune delta parent child = 
  child.f > parent.f +. delta


let f_ordered n1 n2= 
  (**checks to see if 2 nodes are f ordered tie breaking on g*)
  (n1.f < n2.f) ||
    ((n1.f = n2.f) &&
       (n1.g > n2.g))


let ca_beam_search
    ?(queue_record = Fn.no_op2)
    ?(prune_tracker = Fn.no_op1)
    ?(prune_printer = Fn.no_op1)
    ?(record = false)
    ?(weight = 1.0)
    sif
    args =
  (**does a beam search.  Requires the search interface and a list of
     arguments.*)
  let found_nodes = Htable.create sif.Search_interface.hash
    sif.Search_interface.equals 10000 in
  let initial = make_initial sif.Search_interface.initial 
    (sif.Search_interface.h sif.Search_interface.initial) in
  let limit_t = (Limit.make Limit.Nothing sif.Search_interface.halt_on
                   f_ordered
                   (Limit.make_default_logger (fun n -> n.g)
		      (wrap sif.Search_interface.get_sol_length))) in
  let prune_p = match
    Search_args.get_string "Newbeam.beam_search" args 0  with
        "h" -> h_prune
      | "f" -> f_prune
      | _ -> failwith "error - unknown node pruning predicate" in
  let key n = (wrap sif.Search_interface.key) n in

  let expand = record_wrap_expand
    sif.Search_interface.domain_expand
    sif.Search_interface.h
    1.0 in

  let open_list = Queue.create () in
    Queue.add initial open_list;
    Htable.replace found_nodes (key initial) initial;

    if(sif.Search_interface.goal_p initial.data) then 
      (Limit.new_incumbent limit_t
	 (Limit.Incumbent (0., initial)));
    let prune_count = ref 0 in

    let do_search delta = 
      while not (Queue.is_empty open_list) && not (Limit.halt_p limit_t)
	&& (limit_t.Limit.incumbent = Limit.Nothing)
      do
	let to_expand = Queue.take open_list in
	let best_to_expand = Htable.find found_nodes (key to_expand) in
	  if(to_expand.f > best_to_expand.f) then ()
	  else
	    (
	      Limit.incr_exp limit_t;
	      let children = expand to_expand in
		List.iter (
		  fun c ->
		    Limit.incr_gen limit_t;
		    if (sif.Search_interface.goal_p c.data) 
		    then (Limit.new_incumbent limit_t
			    (Limit.Incumbent (0., c)))
		    else if prune_p delta to_expand c
		    then (prune_count := !prune_count + 1)
		    else 
		      (
			(*check if it is in the hash table*)
			try 
			  (
			    let prev_exp = Htable.find found_nodes (key c) in
			      if(prev_exp.f < c.f)
			      then 
				(
				  Htable.replace found_nodes (key c) c;
				  Queue.add c open_list;
				))
			with Not_found -> 
			  (
			    Htable.replace found_nodes (key c) c;
			    Queue.add c open_list;
			  )
		      )
		) children)
      done in

    let delta = ref 10.0 in
    let backtrack_count = ref (-1) in
      while not (Queue.is_empty open_list) && not (Limit.halt_p limit_t)
	&& (limit_t.Limit.incumbent = Limit.Nothing)
      do
	backtrack_count := !backtrack_count + 1;
	do_search !delta;
	delta := !delta /. 2.0;
      done;      

      Datafile.write_pairs stdout 
	["backtracks",(string_of_int !backtrack_count)];
      Datafile.write_pairs stdout 
	["same level",(string_of_int !prune_count)];
      Datafile.write_pairs stdout 
	["deeper replacement",(string_of_int 0)];
      Datafile.write_pairs stdout 
	["shallower replacement",(string_of_int 0)];


      Limit.unwrap_sol6 unwrap_sol_node (Limit.results6 limit_t)


let cab sif args = 
  ca_beam_search sif args

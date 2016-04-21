(**
   Hybrid beam search.  This beam search expands nodes one at a time
   with an expansion limit at each level which is the beam width.
*)


open Search_interface


let closed_index = (-1)


type 'a node = {
  data : 'a; (**)
  f: float;(**)
  g: float;(**)
  d: float;
  depth: int;
  mutable heap_index: int;(*where this node resides in the heap.*)
}


let make_initial sif = 
  let new_h, new_d = sif.Search_interface.hd
    sif.Search_interface.initial in

  {data=sif.Search_interface.initial;
   f=new_h;
   g=0.0;
   d=new_d;
   depth=0;
   heap_index = -1;}


let get_f n =
  n.f

let get_g n =
  n.g

let get_h n =
  n.f -. n.g

let get_d n =
  n.d

let get_depth n = 
  n.depth

let f_ordered n1 n2 = 
  n1.f < n2.f

let h_ordered n1 n2= 
  (n1.f -. n1.g) < (n2.f -. n2.g)

let get_heap_index n = 
  n.heap_index

let unwrap_sol_node s =
  match s with
      Limit.Nothing -> None
    | Limit.Incumbent (q,n) -> Some (n.data, n.g)


let wrap f =
  (fun n -> f n.data)


let wrap2 f =
  (fun a b -> f a.data b.data)



let record_wrap_expand expand h_estimate d_estimate node_record weight
    =
  (fun n ->
     let children = (List.map (fun (d, g) ->
				 if((d_estimate d) < 0.0) then
				   (
				     failwith 
				       (Wrutils.str "d value is negative %f" 
					  (d_estimate d))
				   );
                                 { data = d;
                                   f = ((h_estimate d)*. weight) +. g;
                                   g = g;
				   d = d_estimate d;
				   depth = n.depth + 1;
                                   heap_index = closed_index; })
                       (expand n.data n.g)) in
       node_record n n children;
       children)


let hybrid_beam_search ?(queue_record = Fn.no_op2)
    ?(prune_tracker = Fn.no_op1)
    ?(prune_printer = Fn.no_op1)
    ?(weight = 1.0)
    ?(stratification = "depth")
    ?(node_record = false)
    ?(hard_limit = true)
    ?(bucket_size = 1.0)
    sif
    args =
  let er = 
    match node_record with
	true -> Recorders.expansion_recorder
	  sif.key_printer
	  (fun n -> sif.key n.data)
	  get_g
	  get_depth
	  get_h
      | false -> Fn.no_op4 in

  let prune_same_level = ref 0 in
  let prune_deeper_replacement = ref 0 in
  let prune_shallow_replacement = ref 0 in

  let real_prune_tracker prune_pair = prune_tracker prune_pair;
    let newnode, victim = prune_pair in
      match victim with 
	  Some n -> (
	    if(n.g = newnode.g) then
	      prune_same_level := !prune_same_level + 1
	    else if (n.g < newnode.g) then
	      prune_deeper_replacement := !prune_deeper_replacement + 1
	    else 
	      prune_shallow_replacement := !prune_shallow_replacement + 1
	  )
	| None -> failwith "no victim in pruning transaction" in

  let beam_width = Search_args.get_int "Hybrid_beam.hybrid_beam_search"
    args 0 in

  let key n = (wrap sif.Search_interface.key) n in
  let is_goal n = wrap sif.Search_interface.goal_p n in

  let better_p = f_ordered in

  let closed_list = Htable.create sif.Search_interface.hash
    sif.Search_interface.equals 10000 in
  let initial = make_initial sif in
  let limit_t = (Limit.make Limit.Nothing sif.Search_interface.halt_on
                   f_ordered
                   (Limit.make_default_logger get_g
		      (wrap sif.Search_interface.get_sol_length))) in

  let open_list = Hybrid_beam_queue.create 
    ~update_function:(fun n index -> n.heap_index <- index)
    ~reduce_capacity:(hard_limit)
    f_ordered beam_width initial in

  let expand = record_wrap_expand sif.Search_interface.domain_expand
    sif.Search_interface.h sif.Search_interface.d (er limit_t) weight in


  let get_strata n = match stratification with
      "depth" -> get_depth n 
    | "d" -> (int_of_float ((get_d n) /. bucket_size)) + 1
    | "g" -> (int_of_float (((get_g n) /. bucket_size) +. 1.0))
    | "h" -> (int_of_float ((get_h n) /. bucket_size)) + 1
    | _ -> failwith "invalid strata selection choice"
  in

    Htable.add closed_list (key initial) initial;
    let returned = 
      Hybrid_beam_queue.insert open_list initial (get_strata initial) in
      Verb.pe Verb.never "Hybrid_beam_queue.insert t (%1.20f,ref (-1)) %d;;\n"
	(get_f initial)
	(get_strata initial);
      assert (returned = []);

      if (is_goal initial) then
	(Limit.new_incumbent limit_t (Limit.Incumbent (0., initial)));
      let rec expand_layer () = 
	match limit_t.Limit.incumbent with
	    Limit.Incumbent _ -> ()
	  | Limit.Nothing ->
	      (queue_record limit_t open_list;
	       prune_printer limit_t;
	       assert(Hybrid_beam_queue.check_major_index open_list);
	       assert(Hybrid_beam_queue.check_minor_index open_list get_heap_index);
	       if ((not (Hybrid_beam_queue.empty_p open_list)) &&
		     (not (Limit.halt_p limit_t)))
	       then
		 (let next_node = Hybrid_beam_queue.extract_first open_list in
		    Verb.pe Verb.never "Hybrid_beam_queue.extract_first t;;\n";
		    Limit.incr_exp limit_t;
		    let children = expand next_node in
		    let process_child c = (
		      Limit.incr_gen limit_t;
		      (*node is a goal, just stop*)
		      if(is_goal c) then
			(Limit.new_incumbent limit_t 
			   (Limit.Incumbent (get_g c , c)))
			  (*node is already in the closed list*)
		      else if(Htable.mem closed_list (key c)) then
			(
			  Limit.incr_dups limit_t;
			  let incumbent = Htable.find closed_list (key c) in
			    if(better_p incumbent c) then ()
			    else (if(get_heap_index incumbent = (-1)) then ()
				  else (
				    Verb.pe Verb.never "Hybrid_beam_queue.remove_at t %d %d;;\n"
				      (get_strata incumbent) (get_heap_index incumbent);
				    ignore (Hybrid_beam_queue.remove_at open_list
					      (get_strata incumbent) (get_heap_index incumbent));
				  );
				  Htable.remove closed_list (key incumbent);
				  Htable.add closed_list (key c) c;
				  let pruned = Hybrid_beam_queue.insert open_list
				    c (get_strata c) in
				    Verb.pe Verb.never "Hybrid_beam_queue.insert t (%1.20f,ref(-1)) %d;;\n"
				      (get_f c)
				      (get_strata c);

				    List.iter (fun n -> real_prune_tracker (c, (Some n));
						 Htable.remove closed_list (key n)) pruned;))
			  (*new node*)
		      else
			(Htable.add closed_list (key c) c;
			 let pruned = Hybrid_beam_queue.insert
			   open_list c (get_strata c) in
			   Verb.pe Verb.never "Hybrid_beam_queue.insert t (%1.20f,ref(-1)) %d;;\n"
			     (get_f c)
			     (get_strata c);
			   List.iter (fun n -> real_prune_tracker (n, (Some n));
					Htable.remove closed_list (key n)) pruned;
			)
		    ) in
		      List.iter process_child children;
		      expand_layer ();
		 );
	      ) in
	expand_layer ();
	Datafile.write_pairs stdout 
	  ["same level",(string_of_int !prune_same_level)];
	Datafile.write_pairs stdout 
	  ["deeper replacement",(string_of_int !prune_deeper_replacement)];
	Datafile.write_pairs stdout 
	  ["shallower replacement",(string_of_int !prune_shallow_replacement)];

	Limit.unwrap_sol6 unwrap_sol_node(Limit.results6 limit_t)


let hbs sif args = 
  hybrid_beam_search sif args

let hbs_stratification sif args = 
  hybrid_beam_search ~stratification:(args.(1)) sif args

let bucketed_hbs_stratification sif args = 
  hybrid_beam_search ~stratification:(args.(1))
    ~bucket_size:(float_of_string args.(2)) sif args


let hbs_record sif args = 
  hybrid_beam_search ~stratification:(args.(1)) ~node_record:true sif args

let bucketed_hbs_record sif args = 
  hybrid_beam_search ~stratification:(args.(1)) 
    ~bucket_size:(float_of_string args.(2)) 
    ~node_record:true sif args
   

let leniant_hbs_stratification sif args = 
  hybrid_beam_search ~stratification:(args.(1)) ~hard_limit:false  sif args

let bucketed_leniant_hbs_stratification sif args = 
  hybrid_beam_search ~stratification:(args.(1))
    ~bucket_size:(float_of_string args.(2)) 
    ~hard_limit:false sif args


let bucketed_wted_hbs_stratification sif args = 
  hybrid_beam_search ~stratification:(args.(1))
    ~weight:(float_of_string args.(3)) 
    ~bucket_size:(float_of_string args.(2)) sif args



let bucketed_wted_leniant_hbs_stratification sif args = 
  hybrid_beam_search ~stratification:(args.(1))
    ~bucket_size:(float_of_string args.(2)) 
    ~weight:(float_of_string args.(3)) 
    ~hard_limit:false sif args

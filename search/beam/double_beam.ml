open Newbeam


(* 

   beam searches now want:

   beam width in argument 0
   ordering predicate in argument 1

*)
let double_beam_search
    ?(queue_record = no_op2)
    ?(prune_tracker = no_op1)
    ?(prune_printer = no_op1)
    ?(record = false)
    ?(restart = Restart_none)
    ?(weight = 1.0)
    sif
    args =
  (**does a beam search.  Requires the search interface and a list of
     arguments.*)
  let beam_width = ref 3 in
  let closed_list = Htable.create sif.Search_interface.hash
    sif.Search_interface.equals 10000 in
  let initial = make_initial sif.Search_interface.initial in
  let limit_t = (Limit.make Limit.Nothing sif.Search_interface.halt_on
                   f_ordered
                   (Limit.make_default_logger (fun n -> n.g)
		      (wrap sif.Search_interface.get_sol_length))) in
  let better_p = match
    Search_args.get_string "Newbeam.beam_search" args 0  with
        "h" -> h_ordered
      | "f" -> f_ordered
      | _ -> failwith "error - unknown node sorting predicate" in

  let create_beam () = Picky_queue.create
    ~update_function:(fun n index -> n.heap_index <- index)
    better_p
    (fun n -> n.f)
    !beam_width initial in

  let open_list = create_beam () in
  let key n = (wrap sif.Search_interface.key) n in
  let ht_add n = Htable.replace closed_list (key n) n in
  let ht_check n = Htable.mem closed_list (key n) in
  let ht_remove n = Htable.remove closed_list (key n) in
  let ht_find n = Htable.find closed_list (key n) in
  let is_goal n = wrap sif.Search_interface.goal_p n in

  let exp_recorder = match record with
      true -> let er = Recorders.expansion_recorder
        sif.Search_interface.key_printer
        (fun n -> sif.Search_interface.key n.data)
        (fun n -> n.g)
        (fun n -> 0)
        (fun n -> (n.f -. n.g)) in
        (er limit_t)
    | false -> no_op3 in

  let expand_node = record_wrap_expand sif.Search_interface.domain_expand
    sif.Search_interface.h exp_recorder (-1) weight false in
  let rec expand_layer parent_nq downward_progress =
    (*if there is an incumbent stop*)
    if(limit_t.Limit.incumbent != Limit.Nothing) then (downward_progress)
      (*if the queue is empty then stop*)
    else if(Picky_queue.empty_p parent_nq) then (downward_progress)
      (*if some computation limit has been reached then stop*)
    else if(Limit.halt_p limit_t) then (downward_progress)
      (*process this queue*)
    else
      (
        queue_record limit_t parent_nq;
        prune_printer limit_t;
        let next_layer = create_beam () in
        let expand_this_layer = make_expand ht_add ht_check ht_find
	  ht_remove
	  (fun n -> n.heap_index)
          (*open_add*)
          (Picky_queue.insert next_layer)
          (replace_at parent_nq next_layer
	     (wrap2 (fun n1 n2 -> sif.Search_interface.equals
		       (sif.Search_interface.key n1 )
		       (sif.Search_interface.key n2 ))))
          (*open_replace_at*)
          is_goal expand_node limit_t better_p
          prune_tracker
          no_op1 
	  (fun () -> true)
	  (fun _ -> ())
	in
	  while(not(Picky_queue.empty_p parent_nq) && 
		  not(Limit.halt_p limit_t) &&
		  limit_t.Limit.incumbent = Limit.Nothing)
	  do
	    expand_this_layer (Picky_queue.extract_first parent_nq);
	  done;
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
      while(limit_t.Limit.incumbent = Limit.Nothing && 
	  not(Limit.halt_p limit_t))
      do
	backtrack_count := !backtrack_count + 1;
	Htable.clear closed_list;
	beam_width := !beam_width * 2;
	let new_beam = create_beam () in
	  ht_add initial;
	  ignore(Picky_queue.insert new_beam initial);
	  ignore(expand_layer new_beam 1.);
      done;
      Datafile.write_pairs stdout 
	["backtracks",(string_of_int !backtrack_count)];
      Limit.unwrap_sol6 unwrap_sol_node (Limit.results6 limit_t)


let run_double_beam_search sif args = 
  double_beam_search sif args

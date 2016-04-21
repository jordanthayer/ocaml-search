let unpackage_expand ef =
  fun n -> snd (ef n)


let fd_to_hd f = fun n -> fst (f n)


let adaptive_beam
    ?(queue_record = Newbeam.no_op2)
    ?(prune_tracker = Newbeam.no_op1)
    ?(prune_printer = Newbeam.no_op1)
    ?(record = false)
    ?(weight = None)
    sface args ordered_p better_p setpos getpos =
  let beam_width = Search_args.get_int "Adaptive_beam.adaptive_beam"
    args 0 in

  let closed_list = Htable.create sface.Search_interface.hash
    sface.Search_interface.equals 10000 in
  let initial = sface.Search_interface.initial in
  let limit_t = (Limit.make Limit.Nothing sface.Search_interface.halt_on
                   ordered_p
                   (Limit.make_default_logger ~silent:true (fun n -> n.Single_step.g)
		      (sface.Search_interface.get_sol_length))) in

  let create_beam () = Picky_queue.create
    ~update_function:setpos
    better_p
    (fun n -> n.Single_step.est_f)
    beam_width initial in
  let open_list = create_beam () in
  let key n = (sface.Search_interface.key) n in
  let ht_add n = Htable.replace closed_list (key n) n in
  let ht_check n = Htable.mem closed_list (key n) in
  let ht_remove n = Htable.remove closed_list (key n) in
  let ht_find n = Htable.find closed_list (key n) in
  let is_goal n = sface.Search_interface.goal_p n in

  let open_add c =
    (*hit goal*)
    if(is_goal c) then (Limit.new_incumbent limit_t (Limit.Incumbent (0., c)))
      (*already found a solution*)
    else if (limit_t.Limit.incumbent != Limit.Nothing) then ()
      (*already hit some kind of computation limit*)
    else if (Limit.halt_p limit_t) then ()
      (*child is a duplicate*)
    else if(ht_check c) then (
      Limit.incr_gen limit_t;
      Limit.incr_dups limit_t;
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
              ignore(Picky_queue.replace_at open_list c old_node_index))
	    else
	      (
		ignore(Picky_queue.insert open_list c))))
      (*node is a new node.*)
    else
      (
        Limit.incr_gen limit_t;
        ht_add c;
        ignore(Picky_queue.insert open_list c);
      ) in

  let rec expand_node () =
    (*if there is an incumbent stop*)
    if(limit_t.Limit.incumbent != Limit.Nothing) then ()
      (*if the queue is empty then stop*)
    else if(Picky_queue.empty_p open_list) then ()
      (*if some computation limit has been reached then stop*)
    else if(Limit.halt_p limit_t) then ()
      (*pop one node and process it.*)
    else
      (
        queue_record limit_t open_list;
        prune_printer limit_t;
        (*pull out the node generate the children and put the
	  children into the open list.*)
	let next_node = Picky_queue.extract_first open_list in
	let resort, children = sface.Search_interface.resort_expand
	  next_node in
	  if(resort) then (failwith "don't know how to resort a MMH")
	  else ();
	  List.iter (open_add) children;
	  expand_node ();
      ) in
    (*goal check the initial state, if it is a goal register it as
      incumbent then the other functions will just return.*)
    ignore(Picky_queue.insert open_list initial);
    ht_add initial;
    if(is_goal initial) then (Limit.new_incumbent limit_t
				(Limit.Incumbent (0., initial)));
    (*don't need the return value from the initial beam.*)
    expand_node ();
    Limit.unwrap_sol6 Single_step.unwrap_sol (Limit.results6 limit_t)



let global_hd_ss sface args =
  (** perform a search based on this estimated f function on a domain
      with duplicates.  Never update f estimates or resort queues
      (probably no need since beam searches make new queues so
      often) *)
  let h_calc,f_calc = Global_hd_ss.make_unbounded_correction
    (fun n -> n.Single_step.g)
    (fun n -> n.Single_step.h)
    (fun n -> n.Single_step.d) in
  let new_sif = Single_step.make_interface sface h_calc f_calc
    Timers.reckless in
    adaptive_beam new_sif args Single_step.est_f_then_d_then_g
      Single_step.better_p Single_step.setpos Single_step.getpos


let global_fd_ss sface args =
  (** perform a search based on this estimated f function on a domain
      with duplicates.  Never update f estimates or resort queues
      (probably no need since beam searches make new queues so
      often) *)
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction
    (fun n -> n.Single_step.g)
    (fun n -> n.Single_step.h)
    (fun n -> n.Single_step.g +. n.Single_step.h)
    (fun n -> n.Single_step.d) in
  let new_sif = Single_step.make_interface sface h_calc (fd_to_hd f_calc)
    Timers.reckless in
    adaptive_beam new_sif args Single_step.est_f_then_d_then_g
      Single_step.better_p Single_step.setpos Single_step.getpos


let global_h_ss sface args =
  (** perform a search based on this estimated f function on a domain
      with duplicates.  Never update f estimates or resort queues
      (probably no need since beam searches make new queues so
      often) *)
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction
    (fun n -> n.Single_step.g)
    (fun n -> n.Single_step.h)
    (fun n -> n.Single_step.d) in
  let new_sif = Single_step.make_interface sface h_calc f_calc
    Timers.reckless in
    adaptive_beam new_sif args Single_step.est_f_then_d_then_g
      Single_step.better_p Single_step.setpos Single_step.getpos


let lms_h sface args =
  (** perform a search based on this estimated f function on a domain
      with duplicates.  Never update f estimates or resort queues
      (probably no need since beam searches make new queues so
      often) *)
  let h_calc,f_calc = Lms_h.make_unbounded_correction
    (fun n -> n.Single_step.g)
    (fun n -> n.Single_step.h)
    (fun n -> n.Single_step.d) in
  let new_sif = Single_step.make_interface sface h_calc f_calc
    Timers.reckless in
    adaptive_beam new_sif args Single_step.est_f_then_d_then_g
      Single_step.better_p Single_step.setpos Single_step.getpos


let lms_hd sface args =
  (** perform a search based on this estimated f function on a domain
      with duplicates.  Never update f estimates or resort queues
      (probably no need since beam searches make new queues so
      often) *)
  let h_calc,f_calc = Lms_h.make_unbounded_correction
    (fun n -> n.Single_step.g)
    (fun n -> n.Single_step.h)
    (fun n -> n.Single_step.d) in
  let new_sif = Single_step.make_interface sface h_calc f_calc
    Timers.reckless in
    adaptive_beam new_sif args Single_step.est_f_then_d_then_g
      Single_step.better_p Single_step.setpos Single_step.getpos


let lms_rev_h sface args =
  (** perform a search based on this estimated f function on a domain
      with duplicates.  Never update f estimates or resort queues
      (probably no need since beam searches make new queues so
      often) *)
  let h_calc,f_calc = Lms_rev_h.make_unbounded_correction
    (fun n -> n.Single_step.g)
    (fun n -> n.Single_step.h)
    (fun n -> n.Single_step.d) in
  let new_sif = Single_step.make_interface sface h_calc f_calc
    Timers.reckless in
    adaptive_beam new_sif args Single_step.est_f_then_d_then_g
      Single_step.better_p Single_step.setpos Single_step.getpos

(*
let rev_fd sface args =
  (** perform a search based on this estimated f function on a domain
      with duplicates.  Never update f estimates or resort queues
      (probably no need since beam searches make new queues so
      often) *)
  let h_calc,f_calc = Rev_fd.make_unbounded_correction
    (fun n -> n.Single_step.g)
    (fun n -> n.Single_step.h)
    (fun n -> n.Single_step.d) in
  let new_sif = Single_step.make_interface sface h_calc f_calc
    Timers.reckless in
    adaptive_beam new_sif args Single_step.est_f_then_d_then_g
      Single_step.better_p Single_step.setpos Single_step.getpos
*)

(*
let rev_fd_nomemory sface args =
  (** perform a search based on this estimated f function on a domain
      with duplicates.  Never update f estimates or resort queues
      (probably no need since beam searches make new queues so
      often) *)
  let h_calc,f_calc = Rev_fd_nomemory.make_unbounded_correction
    (fun n -> n.Single_step.g)
    (fun n -> n.Single_step.h)
    (fun n -> n.Single_step.d) in
  let new_sif = Single_step.make_interface sface h_calc (fd_to_hd f_calc)
    Timers.reckless in
    adaptive_beam new_sif args Single_step.est_f_then_d_then_g
      Single_step.better_p Single_step.setpos Single_step.getpos
*)

(*
let rev_fd_globalavg sface args =
  (** perform a search based on this estimated f function on a domain
      with duplicates.  Never update f estimates or resort queues
      (probably no need since beam searches make new queues so
      often) *)
  let h_calc,f_calc = Rev_fd_globalavg.make_unbounded_correction
    (fun n -> n.Single_step.g)
    (fun n -> n.Single_step.h)
    (fun n -> n.Single_step.d) in
  let new_sif = Single_step.make_interface sface h_calc f_calc
    Timers.reckless in
    adaptive_beam new_sif args Single_step.est_f_then_d_then_g
      Single_step.better_p Single_step.setpos Single_step.getpos
*)

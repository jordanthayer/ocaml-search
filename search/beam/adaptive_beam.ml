(*

  runs a basic beam search that can adapt to heuristic error.

*)


let unpackage_expand ef =
  fun n -> snd (ef n)


let adaptive_beam
    ?(queue_record = Newbeam.no_op2)
    ?(prune_tracker = Newbeam.no_op1)
    ?(prune_printer = Newbeam.no_op1)
    ?(record = false)
    ?(restart = Newbeam.Restart_none)
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
        let expand_this_layer = Newbeam.make_expand
	  ht_add
	  ht_check
	  ht_find
	  ht_remove
	  getpos
          (*open_add*)
          (Picky_queue.insert next_layer)
          (Newbeam.replace_at parent_nq next_layer
	     ((fun n1 n2 -> sface.Search_interface.equals
		 (sface.Search_interface.key n1 )
		 (sface.Search_interface.key n2 ))))
          (*open_replace_at*)
          is_goal
	  (unpackage_expand sface.Search_interface.resort_expand)
	  limit_t
	  better_p
          Newbeam.no_op1
          Newbeam.no_op1
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
  let new_sif = Single_step.make_interface sface h_calc
    (Adaptive_bf_beam.fd_to_hd f_calc)
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
  let new_sif = Single_step.make_interface sface h_calc f_calc
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

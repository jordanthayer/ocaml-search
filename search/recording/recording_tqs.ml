(** Tool for recording the executions of three queue search *)

open Three_queue_search_ss_fast


let make_expand recorder expand hd timer calc_h_data f_calc =
  let init = Geq.make_dummy_entry() in
  (fun n ->
     let best_f = ref infinity
     and best_child = ref n
     and reorder = timer() in
     let children = (List.map (fun (s, g) ->
				 let h, d = hd s in
				 let f = g +. h in
				 let c = (make_child s g h d 0.
					    init (n.depth + 1)) in
				   if  f < !best_f then
				     (best_child := c;
				      best_f := f)
				   else if f = !best_f then
				     (if d < !best_child.d then
					(best_child := c;
					 best_f := f));
				   c)
		       (expand n.data n.g))
     in
       if not ((List.length children) = 0)
       then
	 (calc_h_data n !best_child children;
	  List.iter (fun c -> c.est_f <- f_calc c) children);
       recorder n n children;
       reorder,children)


(******************************* Recorders *********************************)

let exp_rec key_printer key =
  Recorders.expansion_recorder key_printer (wrap key)
    (fun n -> n.g) (fun n -> n.depth) (fun n -> n.est_f)

let clean_rec key_printer key =
  let fn = Recorders.dpq_recorder key_printer (wrap key) in
    (fun i dpq geq -> fn i dpq)

let geq_all key_printer key =
  let fn = Recorders.geq_all_recorder key_printer (wrap key) in
    (fun i dpq geq -> fn i geq)

let geq_focal key_printer key =
  let fn = Recorders.geq_focal_recorder key_printer (wrap key) in
    (fun i dpq geq -> fn i geq)

let no_record = (fun _ _ _ -> ())
and no_exp = Recorders.no_node_record

(****************************** Search *************************************)
let make_sface node_record sface bound timer calc_h_data f_calc =
  let init = (make_initial
		sface.Search_interface.initial sface.Search_interface.hd) in
  let isface =
    Search_interface.make
      ~goal_p:(wrap sface.Search_interface.goal_p)
      ~halt_on:sface.Search_interface.halt_on
      ~hash:sface.Search_interface.hash
      ~key:(wrap sface.Search_interface.key)
      ~equals:sface.Search_interface.equals
      sface.Search_interface.domain
      init
      f_order
      (Limit.make_default_logger (fun n -> n.g +. n.h)
	 (wrap sface.Search_interface.get_sol_length)) in
    Search_interface.alter
      ~resort_expand:(Some
			(make_expand
			   (node_record isface.Search_interface.info)
			   sface.Search_interface.domain_expand
			   sface.Search_interface.hd timer calc_h_data f_calc))
      isface

let no_dups ?(node_get = get_node) sface bound timer calc_h_data f_calc
    node_record clean_record geq_record =
  let search_interface =
    make_sface node_record sface bound timer calc_h_data f_calc in
    Limit.unwrap_sol5 unwrap_sol
      (Three_queue_fast.no_dups
	 ~clean_record:clean_record
	 ~geq_record:geq_record
	 search_interface
	 f_order
	 d_then_f_then_g
	 est_f_then_d_then_g
	 (make_close_enough bound)
	 better_p
	 set_qpos
	 get_qpos
	 set_fhpos
	 get_fhpos
	 set_geqe
	 get_geqe
	 (node_get bound)
	 (make_update f_calc))


let dups ?(node_get = get_node) sface bound timer calc_h_data f_calc
    node_record clean_record geq_record =
  let search_interface =
    make_sface node_record sface bound timer calc_h_data f_calc in
    Limit.unwrap_sol6 unwrap_sol
      (Three_queue_fast.dups
	 ~clean_record:clean_record
	 ~geq_record:geq_record
	 search_interface
	 f_order
	 d_then_f_then_g
	 est_f_then_d_then_g
	 (make_close_enough bound)
	 better_p
	 set_qpos
	 get_qpos
	 set_fhpos
	 get_fhpos
	 set_geqe
	 get_geqe
	 (node_get bound)
	 (make_update f_calc))


let delay ?(node_get = get_node_dd) sface bound timer calc_h_data f_calc
    node_record clean_record geq_record =
  let search_interface =
    make_sface node_record sface bound timer calc_h_data f_calc in
    Limit.unwrap_sol6 unwrap_sol
      (Three_queue_fast.delay
	 ~clean_record:clean_record
	 ~geq_record:geq_record
	 search_interface
	 f_order
	 d_then_f_then_g
	 est_f_then_d_then_g
	 (make_close_enough bound)
	 better_p
	 set_qpos
	 get_qpos
	 set_fhpos
	 get_fhpos
	 set_geqe
	 get_geqe
	 set_dpos
	 get_dpos
	 (node_get bound)
	 (make_update f_calc))

(******************* Simplified callers ***********************************)

let expand_record_nodups sface bound timer calc_h_data f_calc key_printer =
  no_dups sface bound timer calc_h_data f_calc
    (exp_rec key_printer (sface.Search_interface.key))
    no_record no_record

let clean_record_nodups sface bound timer calc_h_data f_calc key_printer =
  no_dups sface bound timer calc_h_data f_calc
    no_exp
    (clean_rec key_printer (sface.Search_interface.key))
    no_record

let geq_record_nodups sface bound timer calc_h_data f_calc key_printer =
  no_dups sface bound timer calc_h_data f_calc
    no_exp
    no_record
    (geq_all key_printer (sface.Search_interface.key))

let focal_record_nodups sface bound timer calc_h_data f_calc key_printer =
  no_dups sface bound timer calc_h_data f_calc
    no_exp
    no_record
    (geq_focal key_printer (sface.Search_interface.key))


(*** Dups ***)

let expand_record_dups sface bound timer calc_h_data f_calc key_printer =
  dups sface bound timer calc_h_data f_calc
    (exp_rec key_printer (sface.Search_interface.key))
    no_record no_record

let clean_record_dups sface bound timer calc_h_data f_calc key_printer =
  dups sface bound timer calc_h_data f_calc
    no_exp
    (clean_rec key_printer (sface.Search_interface.key))
    no_record

let geq_record_dups sface bound timer calc_h_data f_calc key_printer =
  dups sface bound timer calc_h_data f_calc
    no_exp
    no_record
    (geq_all key_printer (sface.Search_interface.key))

let focal_record_dups sface bound timer calc_h_data f_calc key_printer =
  dups sface bound timer calc_h_data f_calc
    no_exp
    no_record
    (geq_focal key_printer (sface.Search_interface.key))

(*** Delay ***)

let expand_record_delay sface bound timer calc_h_data f_calc key_printer =
  delay sface bound timer calc_h_data f_calc
    (exp_rec key_printer (sface.Search_interface.key))
    no_record no_record

let clean_record_delay sface bound timer calc_h_data f_calc key_printer =
  delay sface bound timer calc_h_data f_calc
    no_exp
    (clean_rec key_printer (sface.Search_interface.key))
    no_record

let geq_record_delay sface bound timer calc_h_data f_calc key_printer =
  delay sface bound timer calc_h_data f_calc
    no_exp
    no_record
    (geq_all key_printer (sface.Search_interface.key))

let focal_record_delay sface bound timer calc_h_data f_calc key_printer =
  delay sface bound timer calc_h_data f_calc
    no_exp
    no_record
    (geq_focal key_printer (sface.Search_interface.key))


(**************************** calls ***********************************)

let do_rec_h_ss sface bound fn =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    fn sface bound Timers.reckless h_calc f_calc
      sface.Search_interface.key_printer


let h_ss_rec_clean sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_clean" args 0 in
    do_rec_h_ss sface bound clean_record_nodups

and h_ss_rec_exp sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_exp" args 0 in
    do_rec_h_ss sface bound expand_record_nodups

and h_ss_rec_focal sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_focal" args 0 in
    do_rec_h_ss sface bound focal_record_nodups

and h_ss_rec_geq sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_geq" args 0 in
    do_rec_h_ss sface bound geq_record_nodups


let h_ss_rec_clean_dups sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_clean_dups" args 0 in
    do_rec_h_ss sface bound clean_record_dups

and h_ss_rec_exp_dups sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_exp_dups" args 0 in
    do_rec_h_ss sface bound expand_record_dups

and h_ss_rec_focal_dups sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_focal_dups" args 0 in
    do_rec_h_ss sface bound focal_record_dups

and h_ss_rec_geq_dups sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_geq_dups" args 0 in
    do_rec_h_ss sface bound geq_record_dups


let h_ss_rec_clean_delay sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_clean_delay" args 0 in
    do_rec_h_ss sface bound clean_record_delay

and h_ss_rec_exp_delay sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_exp_delay" args 0 in
    do_rec_h_ss sface bound expand_record_delay

and h_ss_rec_focal_delay sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_focal_delay" args 0 in
    do_rec_h_ss sface bound focal_record_delay

and h_ss_rec_geq_delay sface args =
  let bound = Search_args.get_float "Recording_tqs.h_ss_rec_geq_delay" args 0 in
    do_rec_h_ss sface bound geq_record_delay

(* EOF *)

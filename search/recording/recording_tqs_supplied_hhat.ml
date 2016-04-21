(** Tool for recording the eqecution of a three queue search where the
    hhat heuristic has been supplied *)

let h_calc _ _ _ = ()
and hd_calc _ _ _ = ()

let make_fcalc sface =
  let hhat = Three_queue_search_ss.wrap sface.Search_interface.h in
    (fun node ->
       (node.Three_queue_search_ss.g +. (hhat node)))

let make_fdcalc sface =
  let hhat = Three_queue_search_ss_v2.wrap sface.Search_interface.h
  and dhat = Three_queue_search_ss_v2.wrap sface.Search_interface.d in
    (fun node ->
       (node.Three_queue_search_ss_v2.g +. (hhat node)),
       dhat node)



let do_rec_h_ss key_print sface bound fn =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  fn sface bound Timers.reckless h_calc (make_fcalc sface) key_print


let h_ss_rec_clean  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_clean" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.clean_record_nodups

and h_ss_rec_exp  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_exp" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.expand_record_nodups

and h_ss_rec_focal  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_focal" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.focal_record_nodups

and h_ss_rec_geq  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_geq" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.geq_record_nodups


let h_ss_rec_clean_dups  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_clean_dups" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.clean_record_dups

and h_ss_rec_exp_dups  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_exp_dups" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.expand_record_dups

and h_ss_rec_focal_dups  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_focal_dups" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.focal_record_dups

and h_ss_rec_geq_dups  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_geq_dups" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.geq_record_dups


let h_ss_rec_clean_delay  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_clean_delay" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.clean_record_delay

and h_ss_rec_exp_delay  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_exp_delay" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.expand_record_delay

and h_ss_rec_focal_delay  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_focal_delay" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.focal_record_delay

and h_ss_rec_geq_delay  sface args =
  let bound = Search_args.get_float
    "Recording_tqs_supplied_hhat.h_ss_rec_geq_delay" args 0 in
  Recording_tqs.do_rec_h_ss  sface bound
    Recording_tqs.geq_record_delay

(* EOF *)

(** Macros for calling three queue search with various forward looking
    heuristic corrections. *)
open Three_queue_search_ss_fast


let h_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_reckless_dd" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    delay sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_hard_delay sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_reckless_hard_delay"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    delay ~node_get:get_node_hard_delay sface bound Timers.reckless
      h_calc f_calc


let h_ss_geo_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_geo_no_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface bound (Timers.geometric 1. 1.2) h_calc f_calc


let h_ss_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_geo_dups" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface bound (Timers.geometric 1. 1.2) h_calc f_calc


let h_ss_geo_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_geo_dd" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    delay sface bound (Timers.geometric 1. 1.2) h_calc f_calc


let h_ss_geo_hard_delay sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_geo_hard_delay" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    delay ~node_get:get_node_hard_delay sface bound (Timers.geometric 1. 1.2)
      h_calc f_calc


let h_ss_conservative_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_conservative_no_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface bound Timers.conservative h_calc f_calc


let h_ss_conservative_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_conservative_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface bound Timers.conservative h_calc f_calc


let h_ss_conservative_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_conservative_dd" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    delay sface bound Timers.conservative h_calc f_calc



let h_ss_reckless_no_d sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_reckless_no_d" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups ~node_get:no_d_getnode sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_no_d_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_reckless_no_d_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups ~node_get:no_d_getnode sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_no_d_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_reckless_no_d_dd" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    delay ~node_get:no_d_getnode_dd sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_no_fh sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_reckless_no_fh" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups ~node_get:no_fh_getnode sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_no_fh_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_reckless_no_fh_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups ~node_get:no_fh_getnode sface bound Timers.reckless h_calc f_calc


let h_ss_reckless_no_fh_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_ss_reckless_no_fh_dd" args 0 in
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    delay ~node_get:no_fh_getnode_dd sface bound Timers.reckless h_calc f_calc


let h_lms_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_rbn_reckless_no_dups" args 0 in
  let h_calc,f_calc = Lms_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface bound Timers.reckless h_calc f_calc


let h_lms_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast.h_rbn_reckless_dups" args 0 in
  let h_calc,f_calc = Lms_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface bound Timers.reckless h_calc f_calc



(* EOF *)

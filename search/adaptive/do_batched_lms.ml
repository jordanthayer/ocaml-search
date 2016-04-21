(** Very much like tqs_fast.ml but for the version of three queue search which
    corrects both the h and the d estimates of a search *)

open Three_queue_search_ss_v2

let hd_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Do_batched_lms.hd_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    no_dups sface bound Timers.reckless h_calc f_calc


let hd_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Do_batched_lms.hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    dups sface bound Timers.reckless h_calc f_calc


let hd_ss_reckless_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Do_batched_lms.hd_ss_reckless_dd"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    delay sface bound Timers.reckless h_calc f_calc


let h_ar = (fun n -> [|n.h; 1.|]), 2
and d_ar = (fun n -> [|n.d; 1.|]), 2


let hd_lms_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Do_batched_lms.hd_lms_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    h_ar d_ar
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface bound Timers.reckless h_calc f_calc


let hd_lms_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Do_batched_lms.hd_lms_reckless_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    h_ar d_ar
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface bound (Timers.reckless) h_calc f_calc


let hd_lms_reckless_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Do_batched_lms.hd_lms_reckless_dd"
    args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    h_ar d_ar
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    delay sface bound Timers.reckless h_calc f_calc


let hd_batched_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float
    "Do_batched_lms.hd_batched_reckless_no_dups" args 0 in
  let h_calc,f_calc = Batched_lms_hd.make_unbounded_correction
    h_ar d_ar
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface bound Timers.reckless h_calc f_calc


let hd_batched_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Do_batched_lms.hd_batched_reckless_dups"
    args 0 in
  let h_calc,f_calc = Batched_lms_hd.make_unbounded_correction
    h_ar d_ar
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface bound (Timers.reckless) h_calc f_calc


let hd_batched_reckless_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Do_batched_lms.hd_batched_reckless_dd"
    args 0 in
  let h_calc,f_calc = Batched_lms_hd.make_unbounded_correction
    h_ar d_ar
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    delay sface bound Timers.reckless h_calc f_calc


let hdh_lms_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Do_batched_lms.hdh_lms_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.make_unbounded_correction_wdh
    h_ar d_ar
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface bound Timers.reckless h_calc f_calc


let hdh_lms_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Do_batched_lms.hdh_lms_reckless_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.make_unbounded_correction_wdh
    h_ar d_ar
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface bound (Timers.reckless) h_calc f_calc

(* EOF *)

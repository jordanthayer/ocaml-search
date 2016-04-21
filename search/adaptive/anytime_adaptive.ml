(** Calls single step anytime searches - Jordan January 2010 *)

open Single_step

let h_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    anytime_no_dups sface Timers.reckless h_calc f_calc

let h_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    anytime_dups sface Timers.reckless h_calc f_calc


let h_ss_conservative_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    anytime_no_dups sface Timers.conservative h_calc f_calc


let h_ss_conservative_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d)  in
    anytime_dups sface Timers.conservative h_calc f_calc


let h_ss_geo_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d)  in
    anytime_no_dups sface (Timers.geometric 1. 1.2)  h_calc f_calc


let h_ss_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d)  in
    anytime_dups sface (Timers.geometric 1. 1.2) h_calc f_calc


let h_ss_fhp_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "Anytime_adaptive.h_ss_fhp_no_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d)  wt in
    anytime_no_dups sface Timers.reckless h_calc f_calc


let h_ss_fhp_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "Anytime_adaptive.h_ss_fhp_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) wt in
    anytime_dups sface Timers.reckless h_calc f_calc


let h_ss_fhp_conservative_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "Anytime_adaptive.h_ss_fhp_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) wt in
    anytime_no_dups sface Timers.conservative h_calc f_calc


let h_ss_fhp_conservative_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "Anytime_adaptive.h_ss_fhp_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) wt in
    anytime_dups sface Timers.conservative h_calc f_calc


let h_ss_fhp_geo_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "Anytime_adaptive.h_ss_fhp_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) wt in
    anytime_no_dups sface (Timers.geometric 1. 1.2) h_calc f_calc


let h_ss_fhp_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "Anytime_adaptive.h_ss_fhp_dups"
    args 0 in
  let h_calc,f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) wt in
    anytime_dups sface (Timers.geometric 1. 1.2) h_calc f_calc


let hd_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_hd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d)  in
    anytime_no_dups sface Timers.reckless h_calc f_calc


let hd_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_hd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d)  in
    anytime_dups sface Timers.reckless h_calc f_calc

let hd_ss_fhp_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "Anytime_adaptive.h_ss_fhp_dups"
    args 0 in
  let h_calc,f_calc = Global_hd_ss.make_unbounded_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) wt in
    anytime_no_dups sface Timers.reckless h_calc f_calc

let hd_ss_fhp_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "Anytime_adaptive.hd_ss_fhp_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_hd_ss.make_unbounded_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) wt in
    anytime_dups sface Timers.reckless h_calc f_calc

let hd_ss_cfhp_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "Anytime_adaptive.h_ss_fhp_dups"
    args 0 in
  let h_calc,f_calc = Global_hd_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) wt in
    anytime_no_dups sface Timers.reckless h_calc f_calc

let hd_ss_cfhp_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "Anytime_adaptive.hd_ss_fhp_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_hd_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) wt in
    anytime_dups sface Timers.reckless h_calc f_calc


let h_lms_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Lms_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d)  in
    anytime_no_dups sface Timers.reckless h_calc f_calc


let h_lms_fhp_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float
    "Anytime_adaptive.h_lms_fhp_reckless_anytime_no_dups" args 0 in
  let h_calc,f_calc = Lms_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) wt in
    anytime_no_dups sface Timers.reckless h_calc f_calc


let h_lms_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Lms_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d)  in
    anytime_dups sface Timers.reckless h_calc f_calc


let h_lms_fhp_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "Anytime_adaptive.h_lms_fhp_reckless_dups"
    args 0 in
  let h_calc,f_calc = Lms_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d)  wt in
    anytime_dups sface Timers.reckless h_calc f_calc

(* EOF *)

(** Calls single step anytime searches - Jordan January 2010 *)

open Restarting_single_step

let hd_ss_fhp_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "adaptive.h_ss_fhp_dups" args 0 in
  let h_calc,f_calc = Global_hd_ss.make_wted_unbounded_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface Timers.reckless h_calc f_calc wt

let hd_ss_fhp_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "adaptive.hd_ss_fhp_reckless_dups" args 0 in
  let h_calc,f_calc = Global_hd_ss.make_wted_unbounded_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface Timers.reckless h_calc f_calc wt


let hd_ss_cfhp_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "adaptive.h_ss_fhp_dups" args 0 in
  let h_calc,f_calc = Global_hd_ss.make_wted_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface Timers.reckless h_calc f_calc wt

let hd_ss_cfhp_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let wt = Search_args.get_float "adaptive.hd_ss_fhp_reckless_dups" args 0 in
  let h_calc,f_calc = Global_hd_ss.make_wted_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface Timers.reckless h_calc f_calc wt
(* EOF *)

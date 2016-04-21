(** Search calls into EES_lesion3, a lesion of EES needing anytime for
    Bounding - Jordan Feb 2010 *)

open Ees_lesion3


let hd_ss_continued_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d)  in
  let bound = Search_args.get_float "Call_ees_l3.hd_ss_continued_no_dups"
    args 0 in
    continued_no_dups sface bound Timers.reckless h_calc f_calc


let hd_ss_continued_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
  let bound = Search_args.get_float "Call_ees_l3.hd_ss_continued_dups"
    args 0 in
    continued_dups sface bound Timers.reckless h_calc f_calc


let hd_ss_restarting_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d)  in
  let bound = Search_args.get_float "Call_ees_l3.hd_ss_continued_no_dups"
    args 0 in
    restart_no_dups sface bound Timers.reckless h_calc f_calc


let hd_ss_restarting_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
  let bound = Search_args.get_float "Call_ees_l3.hd_ss_continued_dups"
    args 0 in
    restart_dups sface bound Timers.reckless h_calc f_calc


let hd_ss_repairing_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d)  in
  let bound = Search_args.get_float "Call_ees_l3.hd_ss_continued_no_dups"
    args 0 in
    repairing_no_dups sface bound Timers.reckless h_calc f_calc


let hd_ss_repairing_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with many duplicates.  Never update f estimates or resort queues *)
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
  let bound = Search_args.get_float "Call_ees_l3.hd_ss_continued_dups"
    args 0 in
    repairing_dups sface bound Timers.reckless h_calc f_calc
(* EOF *)

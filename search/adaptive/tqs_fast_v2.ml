(** Very much like tqs_fast.ml but for the version of three queue search which
    corrects both the h and the d estimates of a search *)

open Three_queue_search_ss_v2

let hd_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    no_dups sface bound Timers.reckless h_calc f_calc

let hd_pm_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_pm_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    no_dups_pm sface bound Timers.reckless h_calc f_calc

let hd_ss_geo_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_geo_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    no_dups sface bound (Timers.geometric 10. 1.2) h_calc f_calc

let hd_pm_ss_geo_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_pm_ss_geo_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    no_dups_pm sface bound (Timers.geometric 10. 1.2) h_calc f_calc


let hd_ss_conservative_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_conservative_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    no_dups sface bound Timers.conservative h_calc f_calc


let hd_ss_reckless_dups sface args =
(** perform a search based on this estimated f function on a domain
    with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
  dups sface bound Timers.reckless h_calc f_calc


let hd_ss_path_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Path_fd_ss.make_unbounded_correction_austin
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) (fun n -> n.depth)
  (fun n -> n.h_err) (fun n -> n.d_err) in
    dups sface bound Timers.reckless h_calc f_calc


let hd_ss_path_aggressive sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Path_fd_ss.make_unbounded_correction_austin
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) (fun n -> n.depth)
  (fun n -> n.h_err) (fun n -> n.d_err) in
    Ees_cleanup.dups bound sface h_calc f_calc


let hd_ss_path_aggressive_m2 sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Path_fd_ss.make_unbounded_correction_austin
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) (fun n -> n.depth)
  (fun n -> n.h_err) (fun n -> n.d_err) in
    Ees_cleanup.dups2 bound sface h_calc f_calc


 let hd_ss_path_aggressive_m2_pm sface args =
   (** perform a search based on this estimated f function on a domain
       with few duplicates.  Never update f estimates or resort queues *)
   let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dups"
     args 0 in
   let h_calc,f_calc = Path_fd_ss.make_unbounded_correction_austin
     (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) (fun n -> n.depth)
   (fun n -> n.h_err) (fun n -> n.d_err) in
     Ees_cleanup.dups2_pm bound sface h_calc f_calc



let hd_ss_path_pm_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Path_fd_ss.make_unbounded_correction_austin
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) (fun n -> n.depth)
  (fun n -> n.h_err) (fun n -> n.d_err) in
    dups_pm sface bound Timers.reckless h_calc f_calc


let hd_ss_oldstyle_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction_oldschool
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
    dups sface bound Timers.reckless h_calc f_calc


let hd_ss_path_oldschool_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Path_fd_ss.make_unbounded_correction_oldschool
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) (fun n -> n.depth)
  (fun n -> n.h_err) (fun n -> n.d_err) in
    dups sface bound Timers.reckless h_calc f_calc

let hd_pm_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_pm_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    dups_pm sface bound Timers.reckless h_calc f_calc


let hd_ss_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_geo_dups" args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    dups sface bound (Timers.geometric 10. 1.2) h_calc f_calc

let hd_pm_ss_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_pm_ss_geo_dups" args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    dups_pm sface bound (Timers.geometric 10. 1.2) h_calc f_calc


let hd_ss_conservative_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_conservative_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    dups sface bound Timers.conservative h_calc f_calc


let hd_ss_reckless_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dd" args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    delay sface bound Timers.reckless h_calc f_calc


let h_ar () = (let har = [|infinity; 1.|] in
	      (fun n ->  har.(0) <- n.h; har), 2)

(*
let h_ar () = (let har = [|infinity; infinity; 1.|] in
	      (fun n ->  har.(0) <- n.h; har.(1) <- n.d; har), 3)
*)

and d_ar () = (let dar = [|infinity; 1.|] in
		 (fun n -> dar.(0) <-n.d; dar), 2)

(*
let h_ar () = (let har = [|infinity; infinity; infinity; infinity;1.|] in
	      (fun n ->  har.(0) <- n.h;
		 har.(1) <- n.d;
		 har.(2) <- n.g;
		 har), 4)

and d_ar () = (let dar = [|infinity; infinity; infinity; infinity;1.|] in
		 (fun n -> dar.(0) <-n.d;
		    dar.(1) <- n.h;
		    dar.(2) <- float_of_int n.depth;
		    dar), 4)
*)


let hd_lms_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface bound Timers.reckless h_calc f_calc


let hd_lms_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_reckless_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface bound (Timers.reckless) h_calc f_calc


let hd_lms_pm_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_pm_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups_pm sface bound Timers.reckless h_calc f_calc


let hd_lms_pm_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_pm_reckless_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups_pm sface bound (Timers.reckless) h_calc f_calc


let hd_lms_geo_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_geo_no_dups" args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface bound (Timers.geometric 10. 1.2) h_calc f_calc


let hd_lms_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_geo_dups" args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface bound (Timers.geometric 10. 1.2) h_calc f_calc


let hd_lms_pm_geo_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_pm_geo_no_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups_pm sface bound (Timers.geometric 10. 1.2) h_calc f_calc


let hd_lms_pm_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_pm_geo_dups" args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups_pm sface bound (Timers.geometric 10. 1.2) h_calc f_calc


let hd_lms_reckless_dd sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_reckless_dd" args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    delay sface bound Timers.reckless h_calc f_calc


let hdh_lms_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hdh_lms_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.make_unbounded_correction_wdh
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface bound Timers.reckless h_calc f_calc


let hdh_lms_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hdh_lms_reckless_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.make_unbounded_correction_wdh
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface bound (Timers.reckless) h_calc f_calc


let simple_hd_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.simple_hd_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    s_no_dups sface bound Timers.reckless h_calc f_calc


let simple_hd_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.simple_hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    s_dups sface bound Timers.reckless h_calc f_calc


let hd_ss_reckless_no_fh_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_fh_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    no_dups ~node_get:no_fh_getnode sface bound Timers.reckless h_calc f_calc


let hd_ss_reckless_no_fh_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_fh_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    dups ~node_get:no_fh_getnode sface bound Timers.reckless h_calc f_calc


let anytime_hd_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    anytime_no_dups sface bound Timers.reckless h_calc f_calc

let anytime_hd_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.f) (fun n -> n.d) in
    anytime_dups sface bound Timers.reckless h_calc f_calc

(* EOF *)

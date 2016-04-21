(** Very much like tqs_fast.ml but for the version of three queue search which
    corrects both the h and the d estimates of a search *)

open Restarting_tqs

let hd_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    no_dups sface bound Timers.reckless h_calc f_calc

let hd_ss_geo_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_geo_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    no_dups sface bound (Timers.geometric 10. 1.2) h_calc f_calc

let hd_ss_conservative_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_conservative_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    no_dups sface bound Timers.conservative h_calc f_calc


let hd_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    dups sface bound Timers.reckless h_calc f_calc

let hd_ss_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_geo_dups" args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    dups sface bound (Timers.geometric 10. 1.2) h_calc f_calc

let hd_ss_conservative_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_conservative_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    dups sface bound Timers.conservative h_calc f_calc


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

let hd_ss_reckless_no_fh_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_fh_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    no_dups ~node_get:no_fh_getnode sface bound Timers.reckless h_calc f_calc

let hd_ss_reckless_no_fh_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_fh_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    dups ~node_get:no_fh_getnode sface bound Timers.reckless h_calc f_calc



let supplied_h_calc _ _ _ = ()
and make_supplied_fdcalc sface =
  let hhat = wrap sface.Search_interface.h
  and dhat = wrap sface.Search_interface.d in
    (fun node ->
       (node.g +. (hhat node)),
       dhat node)

let supplied_hd_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_dups"
    args 0 in
    no_dups sface bound Timers.reckless supplied_h_calc
      (make_supplied_fdcalc sface)

let supplied_hd_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_dups"
    args 0 in
    dups sface bound Timers.reckless supplied_h_calc
      (make_supplied_fdcalc sface)



(*************************************** Repairing calls *********************)
let repair_supplied_hd_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_dups"
    args 0 in
    repairing_no_dups sface bound Timers.reckless supplied_h_calc
      (make_supplied_fdcalc sface)

let repair_supplied_hd_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_dups"
    args 0 in
    repairing_dups sface bound Timers.reckless supplied_h_calc
      (make_supplied_fdcalc sface)

let repair_hd_ss_reckless_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h)(fun n -> n.g +. n.h) (fun n -> n.d) in
    repairing_no_dups sface bound Timers.reckless h_calc f_calc

let repair_hd_ss_geo_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_geo_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    repairing_no_dups sface bound (Timers.geometric 10. 1.2) h_calc f_calc

let repair_hd_ss_conservative_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_conservative_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    repairing_no_dups sface bound Timers.conservative h_calc f_calc

let repair_hd_ss_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    repairing_dups sface bound Timers.reckless h_calc f_calc

let repair_hd_ss_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_geo_dups" args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    repairing_dups sface bound (Timers.geometric 10. 1.2) h_calc f_calc

let repair_hd_ss_conservative_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_conservative_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    repairing_dups sface bound Timers.conservative h_calc f_calc

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

let repair_hd_lms_reckless_repairing_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    repairing_no_dups sface bound Timers.reckless h_calc f_calc

let repair_hd_lms_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_reckless_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    repairing_dups sface bound (Timers.reckless) h_calc f_calc

let repair_hd_lms_geo_repairing_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_geo_no_dups" args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    repairing_no_dups sface bound (Timers.geometric 10. 1.2) h_calc f_calc

let repair_hd_lms_geo_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_lms_geo_dups" args 0 in
  let h_calc,f_calc = Lms_hd.gen_make_unbounded_correction
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    repairing_dups sface bound (Timers.geometric 10. 1.2) h_calc f_calc

let repair_hdh_lms_reckless_repairing_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hdh_lms_reckless_no_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.make_unbounded_correction_wdh
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    repairing_no_dups sface bound Timers.reckless h_calc f_calc

let repair_hdh_lms_reckless_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hdh_lms_reckless_dups"
    args 0 in
  let h_calc,f_calc = Lms_hd.make_unbounded_correction_wdh
    (h_ar ()) (d_ar ())
    (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    repairing_dups sface bound (Timers.reckless) h_calc f_calc

let repair_hd_ss_reckless_no_fh_repairing_no_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_fh_no_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    repairing_no_dups ~node_get:no_fh_getnode sface bound Timers.reckless h_calc f_calc

let repair_hd_ss_reckless_no_fh_dups sface args =
  (** perform a search based on this estimated f function on a domain
      with few duplicates.  Never update f estimates or resort queues *)
  let bound = Search_args.get_float "Tqs_fast_v2.hd_ss_reckless_no_fh_dups"
    args 0 in
  let h_calc,f_calc = Global_fd_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.g +. n.h) (fun n -> n.d) in
    repairing_dups ~node_get:no_fh_getnode sface bound Timers.reckless h_calc f_calc

(* EOF *)



(** Various learning searches which use the reorderable cleanup framework
    to explore nodes in what may very well be a loose order, then prove the
    suboptimality bound after the fact *)

open Single_step_cleanup


let cleanup_no_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.cleanup_no_dups"
    args 0 in
  let h_calc, f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    no_dups sface bound Timers.reckless h_calc f_calc

and cleanup_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.cleanup_dups" args 0 in
  let h_calc, f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    dups sface bound Timers.reckless h_calc f_calc

and cleanup_dd sface args =
  let bound = Search_args.get_float "Cleanup_searches.cleanup_dd" args 0 in
  let h_calc, f_calc = Global_h_ss.make_unbounded_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) in
    delay_dups sface bound Timers.reckless h_calc f_calc


let ca_cleanup_no_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.ca_cleanup_no_dups"
    args 0
  and optimism = Search_args.get_float "Cleanup_searches.ca_cleanup_no_dups"
    args 0 in
  let h_calc, f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) (bound *. optimism) in
    no_dups sface bound Timers.reckless h_calc f_calc

and ca_cleanup_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.ca_cleanup_dups"
    args 0
  and optimism = Search_args.get_float "Cleanup_searches.ca_cleanup_dups"
    args 0 in
  let h_calc, f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) (bound *. optimism) in
    dups sface bound Timers.reckless h_calc f_calc

and ca_cleanup_dd sface args =
  let bound = Search_args.get_float "Cleanup_searches.ca_cleanup_dd" args 0
  and optimism = Search_args.get_float "Cleanup_searches.ca_cleanup_dd"
    args 0 in
  let h_calc, f_calc = Global_h_ss.make_clamped_fhp_correction (fun n -> n.g)
    (fun n -> n.h) (fun n -> n.d) (bound *. optimism) in
    delay_dups sface bound Timers.reckless h_calc f_calc



let fd_cleanup_no_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_no_dups"
    args 0 in
  let hd_calc, fd_calc = Global_fd_ss.make_unbounded_correction_prime
    bound (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
    no_dups_fd sface bound Timers.reckless hd_calc fd_calc

and fd_cleanup_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_dups"
    args 0 in
  let hd_calc, fd_calc = Global_fd_ss.make_unbounded_correction_prime
    bound (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
    dups_fd sface bound Timers.reckless hd_calc fd_calc

and fd_cleanup_dd sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_dd" args 0 in
  let hd_calc, fd_calc = Global_fd_ss.make_unbounded_correction_prime
    bound (fun n -> n.g)  (fun n -> n.h) (fun n -> n.d) in
    delay_dups_fd sface bound Timers.reckless hd_calc fd_calc

and fd_cleanup_pm_no_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_pm_no_dups"
    args 0 in
  let hd_calc, fd_calc = Global_fd_ss.make_unbounded_correction_prime_noneg
    bound (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
    no_dups_fd sface bound Timers.reckless hd_calc fd_calc

and fd_cleanup_pm_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_pm_dups"
    args 0 in
  let hd_calc, fd_calc = Global_fd_ss.make_unbounded_correction_prime_noneg
    bound (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) in
    dups_fd sface bound Timers.reckless hd_calc fd_calc


let fd_fixed_cleanup_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_dups"
    args 0 in
  let h = wrap sface.Search_interface.h
  and d = wrap sface.Search_interface.d in
  let hd_calc _ _ _  = ()
  and fd_calc = (fun n -> n.g +. bound *. (h n) , d n) in
    dups_fd sface bound Timers.reckless hd_calc fd_calc


(*****************************************************************************)

let lms_fd_cleanup_no_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_no_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let hd_calc, fd_calc = Lms_h.make_fhp_correction [|1.; 0.; 0.; 0.;|]
    maxes.(Norm_values.g) maxes.(Norm_values.h) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) bound in
    no_dups_fd sface bound Timers.reckless hd_calc fd_calc

and lms_fd_cleanup_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let hd_calc, fd_calc = Lms_h.make_fhp_correction [|1.; 0.; 0.; 0.;|]
    maxes.(Norm_values.g) maxes.(Norm_values.h) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) bound in
    dups_fd sface bound Timers.reckless hd_calc fd_calc

and lms_fd_cleanup_dd sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_dd" args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let hd_calc, fd_calc = Lms_h.make_fhp_correction [|1.; 0.; 0.; 0.;|]
    maxes.(Norm_values.g) maxes.(Norm_values.h) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) bound in
    delay_dups_fd sface bound Timers.reckless hd_calc fd_calc


let ann_fd_cleanup_no_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_no_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let hd_calc, fd_calc = Ann_hd.make_batch_delay_fhp_correction 100 bound
    maxes maxes.(Norm_values.h) (fun n -> [| n.h; n.d; n.g; 1.|])
    (fun n -> n.g) (fun n -> n.h) in
    no_dups_fd sface bound Timers.reckless hd_calc (fun n -> fd_calc n, nan)

and ann_fd_cleanup_dups sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in

  let hd_calc, fd_calc = Ann_hd.make_batch_delay_fhp_correction 100 bound
    maxes maxes.(Norm_values.h) (fun n -> [| n.h; n.d; n.g; 1.|])
    (fun n -> n.g) (fun n -> n.h) in
    dups_fd sface bound Timers.reckless hd_calc (fun n -> fd_calc n, nan)

and ann_fd_cleanup_dd sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_dd" args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in

  let hd_calc, fd_calc = Ann_hd.make_batch_delay_fhp_correction 100 bound
    maxes maxes.(Norm_values.h) (fun n -> [| n.h; n.d; n.g; 1.|])
    (fun n -> n.g) (fun n -> n.h) in
    delay_dups_fd sface bound Timers.reckless hd_calc (fun n -> fd_calc n, nan)

(********************* Fixed LMS Code ****************************************)

let lms_fd_cleanup_no_dups_no_learn sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_no_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Cleanup Lms" args 1;
			  Search_args.get_float "Cleanup Lms" args 2;
			  Search_args.get_float "Cleanup Lms" args 3;
			  Search_args.get_float "Cleanup Lms" args 4;|]) in
  let _, fd_calc = Lms_h.make_fhp_correction i_weights
    maxes.(Norm_values.g) maxes.(Norm_values.h) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) bound in
    no_dups_fd sface bound Timers.reckless (fun _ _ _ -> nan) fd_calc

and lms_fd_cleanup_dups_no_learn sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_dups"
    args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Cleanup Lms" args 1;
			  Search_args.get_float "Cleanup Lms" args 2;
			  Search_args.get_float "Cleanup Lms" args 3;
			  Search_args.get_float "Cleanup Lms" args 4;|]) in
  let _, fd_calc = Lms_h.make_fhp_correction i_weights
    maxes.(Norm_values.g) maxes.(Norm_values.h) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) bound in
    dups_fd sface bound Timers.reckless (fun _ _ _ -> nan) fd_calc

and lms_fd_cleanup_dd_no_learn sface args =
  let bound = Search_args.get_float "Cleanup_searches.fd_cleanup_dd" args 0 in
  let maxes = Norm_values.weighted_norm_from_sface sface 2.5 in
  let i_weights = (if args = [||]
		   then [|1.; 0.; 0.; 0.|]
		   else [|Search_args.get_float "Cleanup Lms" args 1;
			  Search_args.get_float "Cleanup Lms" args 2;
			  Search_args.get_float "Cleanup Lms" args 3;
			  Search_args.get_float "Cleanup Lms" args 4;|]) in
  let _, fd_calc = Lms_h.make_fhp_correction i_weights
    maxes.(Norm_values.g) maxes.(Norm_values.h) maxes.(Norm_values.d)
    (fun n -> n.g) (fun n -> n.h) (fun n -> n.d) bound in
    delay_dups_fd sface bound Timers.reckless (fun _ _ _ -> nan) fd_calc

(* EOF *)
